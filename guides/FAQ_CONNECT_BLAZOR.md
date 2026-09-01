---
title: "FAQ: Connecting a Blazor Website to the Mesh"
layer: guide
audience: [agent, human]
stage: draft
---

# FAQ: How Do I Connect My Blazor Website to the Mesh?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

**No prior art for this exists anywhere in this workspace.** Everything
below is reasoned from [`macula-dotnet`](https://github.com/macula-io/macula-dotnet)'s
real, verified API (see
[FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md))
and from documented, general .NET/browser platform constraints — not from
a working example. Treat this page as a starting design, not a tested
recipe, and update it once someone actually builds this.

---

## The one thing that decides everything: which Blazor

"Blazor" names two genuinely different execution models, and they are not
equally feasible here — this isn't a nuance, it's the whole answer.

| | Blazor **Server** | Blazor **WebAssembly** |
|---|---|---|
| Where your C# actually runs | On the server, in a normal ASP.NET Core process | Compiled to WASM, executing **inside the browser sandbox** |
| Browser's role | Thin client: SignalR carries UI events up, UI diffs down | Runs the whole app — no server round-trip for logic |
| Can it use `macula-dotnet` directly? | **Yes** — it's ordinary server-side .NET | **No** — see below, and this is not a temporary gap |

## Blazor Server — should work, same as any other server-side .NET app

Because Blazor Server's component code executes on the server, using
`macula-dotnet` here is architecturally no different from using it in a
plain ASP.NET Core API — full `System.Net.Quic`, the `Unofficial.MsQuic`
native library, `BouncyCastle.Cryptography`, all genuinely available,
because this is a real, unrestricted .NET process. Following the same
shape `macula-dotnet`'s own README examples use:

```bash
dotnet add package Macula
```

```csharp
// MeshConnectionService.cs — one Session shared across every connected
// browser circuit, registered as a singleton.
public sealed class MeshConnectionService : IAsyncDisposable
{
    private Session? _session;

    public async Task<Session> GetSessionAsync(string host, int port)
    {
        if (_session is not null) return _session;
        var identity = KeyPair.GenerateWithDefaultPuzzle();
        _session = await Session.ConnectAsync(host, port, identity, Trust.UseWebPki);
        return _session;
    }

    public async ValueTask DisposeAsync()
    {
        if (_session is not null) await _session.DisposeAsync();
    }
}
```

```csharp
// Program.cs
builder.Services.AddSingleton<MeshConnectionService>();
```

```razor
@* A component reacting to mesh events, StateHasChanged-driven — the
   Blazor Server equivalent of Phoenix LiveView's PubSub.subscribe +
   handle_info pattern (see FAQ_CONNECT_PHOENIX_LIVEVIEW.md). *@
@inject MeshConnectionService Mesh
@implements IAsyncDisposable

<p>@_lastPayload</p>

@code {
    private string? _lastPayload;

    protected override async Task OnInitializedAsync()
    {
        var session = await Mesh.GetSessionAsync("station-de-frankfurt.macula.io", 4433);
        // subscribe/watch call here; on each event:
        //   _lastPayload = payload;
        //   await InvokeAsync(StateHasChanged);
    }

    public ValueTask DisposeAsync() => ValueTask.CompletedTask; // unsubscribe here
}
```

**Design decision worth making deliberately, not by default**: one shared
app-wide `Session` (above — efficient, one mesh identity for the whole
site, fine when the site itself is the mesh participant) versus one
`Session` per circuit (one mesh identity per browser tab — only worth the
cost if each visitor genuinely needs to appear as a distinct mesh
identity). There's no existing precedent here to defer to; pick based on
what your service actually needs to represent on the mesh.

## Blazor WebAssembly — cannot connect directly, and this isn't temporary

This is a hard platform limitation, not a "nobody's built it yet" gap:

- **`System.Net.Quic` does not exist for the `browser-wasm` target at
  all.** This is a documented .NET platform boundary — QUIC needs a real
  OS-level UDP socket and a native QUIC implementation underneath it
  (msquic), neither of which the WASM sandbox exposes to managed code
  running inside a browser tab.
- **`Unofficial.MsQuic` (the native shared library `macula-dotnet` uses
  for Linux QUIC support) is real machine code.** A WASM app cannot
  P/Invoke into an arbitrary native shared library the way a
  server-process `.dll`/`.so` load can — the browser sandbox simply
  doesn't allow it.
- This is the identical reason [`macula-cli`](FAQ_MACULA_CLI.md) and
  [`macula-mcp`](FAQ_MACULA_MCP.md) are separate OS processes rather than
  something embeddable in a page: raw QUIC needs real socket access that
  no browser grants to any sandboxed code, WASM or JavaScript alike.

**The only viable pattern**: don't try to make the WASM client a mesh
peer. Put a real server-side process between the browser and the mesh —
which, if you're already building a Blazor Server-adjacent backend, is
the *same* `MeshConnectionService` pattern above, just exposed to the
WASM client over ordinary browser-legal transport (a minimal API
endpoint, or SignalR for push) instead of Blazor Server's built-in
server-rendering:

```
Blazor WebAssembly (browser)  --HTTP/SignalR-->  ASP.NET Core backend  --QUIC-->  macula-station
        (no macula-dotnet here)                  (macula-dotnet lives here)
```

This is the same backend-for-frontend shape any browser-based client
needing mesh access has to use — a mobile app has an equivalent
constraint for the same underlying reason (browsers and, similarly,
strict mobile OS sandboxes don't hand out raw QUIC to arbitrary app code
without real platform-level socket support); see
[FAQ: Building Mobile Applications for the Mesh](FAQ_BUILD_MOBILE_APPS.md).

## If you actually build this

This page is a design sketch, not a verified guide — please correct it
once real code exists. Things worth verifying first, that this page
cannot answer without a real build:
- Whether `Unofficial.MsQuic` (or Microsoft's own MsQuic) has any
  server-hosting-environment gotchas under IIS/Kestrel process models
  commonly used for Blazor Server deployment.
- Circuit-loss behavior: Blazor Server can lose and reconnect a circuit
  (network blip, tab backgrounded) independently of the underlying mesh
  `Session` — decide whether a reconnecting circuit should reuse the
  existing singleton `Session` (likely correct for the shared-session
  design above) or needs its own reconciliation logic.

## See also

- [FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md) — the real, verified `macula-dotnet` API this page builds on
- [FAQ: Connecting Phoenix LiveView to the Mesh](FAQ_CONNECT_PHOENIX_LIVEVIEW.md) — the equivalent pattern with real prior art
- [FAQ: Building Mobile Applications for the Mesh](FAQ_BUILD_MOBILE_APPS.md) — the same browser/sandbox constraint, mobile-flavored
