// Core's Timezone js loader resolves zone names through the Temporal API:
// it uses globalThis.Temporal, falling back to
// globalThis.TemporalPolyfill.Temporal. In a browser without native
// Temporal every zone lookup fails, and Bonsai_web.Start forces
// Timezone.local (to timestamp console logs), so the whole app dies at
// startup with ("unknown zone" (zone <local tz>)). Point core's fallback
// slot at the vendored polyfill, which installed itself as
// globalThis.temporal in the previous linked file.
if (typeof globalThis.Temporal === "undefined"
    && typeof globalThis.TemporalPolyfill === "undefined"
    && globalThis.temporal) {
  globalThis.TemporalPolyfill = { Temporal: globalThis.temporal.Temporal };
}
