open! Core
open! Async
open! Types

type verdict =
  { is_match : bool
  ; explanation : string
  }
[@@deriving sexp_of]

module Pair_key = struct
  type t = Market_id.t * Market_id.t [@@deriving compare, hash, sexp_of]
end

let cache_key (candidate : Matcher.Candidate.t) : Pair_key.t =
  candidate.left.market_id, candidate.right.market_id
;;

let cache : (Pair_key.t, verdict) Hashtbl.t =
  Hashtbl.create (module Pair_key)
;;

let messages_uri = Uri.of_string "https://api.anthropic.com/v1/messages"
let anthropic_version = "2023-06-01"
let model = "claude-haiku-4-5"
let max_tokens = 1024

let request_body (candidate : Matcher.Candidate.t) : Yojson.Safe.t =
  let describe (stub : Market_stub.t) =
    let close_time =
      Option.value_map
        stub.close_time
        ~default:"unknown"
        ~f:Time_ns.to_string_utc
    in
    [%string
      "venue: %{stub.venue#Venue}; title: %{stub.title}; closes: \
       %{close_time}"]
  in
  let system_prompt =
    "You decide whether two prediction markets settle on the same \
     real-world event. Two markets match only if every outcome that \
     resolves one YES would necessarily resolve the other YES: the same \
     subject, the same measurement or source, the same numeric threshold, \
     and the same deadline. Different thresholds (e.g. $100K vs $95K), \
     different dates, or different resolution sources mean NOT a match, no \
     matter how similar the titles read. Wording differences alone never \
     matter. If the descriptions leave you genuinely unsure, answer that \
     they do not match: a false match risks money, a missed match only \
     misses an opportunity. Keep the explanation to one or two sentences \
     naming the attribute that drove your decision."
  in
  let prompt =
    [%string
      "Do these two prediction markets settle on the same real-world event?\n\
       Market A: %{describe candidate.left}\n\
       Market B: %{describe candidate.right}"]
  in
  let format =
    `Assoc
      [ "type", `String "json_schema"
      ; ( "schema"
        , `Assoc
            [ "type", `String "object"
            ; ( "properties"
              , `Assoc
                  [ "is_match", `Assoc [ "type", `String "boolean" ]
                  ; "explanation", `Assoc [ "type", `String "string" ]
                  ] )
            ; "required", `List [ `String "is_match"; `String "explanation" ]
            ; "additionalProperties", `Bool false
            ] )
      ]
  in
  `Assoc
    [ "model", `String model
    ; "max_tokens", `Int max_tokens
    ; "system", `String system_prompt
    ; ( "messages"
      , `List
          [ `Assoc [ "role", `String "user"; "content", `String prompt ] ] )
    ; "output_config", `Assoc [ "format", format ]
    ]
;;

let parse_verdict (response_body : string) : verdict Or_error.t =
  Or_error.tag
    ~tag:"could not parse model response"
    (Or_error.try_with (fun () ->
       let open Yojson.Safe.Util in
       let response = Yojson.Safe.from_string response_body in
       (match member "type" response with
        | `String "error" ->
          let error_message =
            response |> member "error" |> member "message" |> to_string
          in
          raise_s [%message "API error" (error_message : string)]
        | _ -> ());
       (match response |> member "stop_reason" |> to_string with
        | "end_turn" -> ()
        | stop_reason ->
          raise_s [%message "unexpected stop_reason" (stop_reason : string)]);
       let text =
         response
         |> member "content"
         |> to_list
         |> List.find_map ~f:(fun block ->
           match member "type" block with
           | `String "text" -> Some (block |> member "text" |> to_string)
           | _ -> None)
       in
       match text with
       | None -> raise_s [%message "no text block in response content"]
       | Some text ->
         let fields = Yojson.Safe.from_string text in
         { is_match = fields |> member "is_match" |> to_bool
         ; explanation = fields |> member "explanation" |> to_string
         }))
;;

let adjudicate (candidate : Matcher.Candidate.t)
  : verdict Or_error.t Deferred.t
  =
  let key = cache_key candidate in
  match Hashtbl.find cache key with
  | Some verdict -> Deferred.Or_error.return verdict
  | None ->
    (match Sys.getenv "ANTHROPIC_API_KEY" with
     | None ->
       Deferred.Or_error.error_string
         "ANTHROPIC_API_KEY is not set; export it before running (e.g. set \
          -a; source .env; set +a)"
     | Some api_key ->
       let headers =
         Cohttp.Header.of_list
           [ "content-type", "application/json"
           ; "x-api-key", api_key
           ; "anthropic-version", anthropic_version
           ]
       in
       let request =
         Cohttp_async.Body.of_string
           (Yojson.Safe.to_string (request_body candidate))
       in
       let%bind response =
         Monitor.try_with_or_error (fun () ->
           let%bind response, body =
             Cohttp_async.Client.post ~headers ~body:request messages_uri
           in
           let%map body = Cohttp_async.Body.to_string body in
           Cohttp.Response.status response, body)
       in
       let verdict =
         Or_error.bind response ~f:(fun (status, body) ->
           match status with
           | #Cohttp.Code.success_status -> parse_verdict body
           | status ->
             Or_error.error_s
               [%message
                 "http request failed"
                   (Cohttp.Code.string_of_status status : string)
                   (body : string)])
       in
       (match verdict with
        | Ok verdict -> Hashtbl.set cache ~key ~data:verdict
        | Error _ -> ());
       return verdict)
;;

module For_testing = struct
  let cache_key = cache_key
  let request_body = request_body
  let parse_verdict = parse_verdict
  let cache = cache
end
