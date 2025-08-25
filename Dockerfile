# =========================
# Dockerfile (multi-stage)
# =========================

# ---- Builder ----
FROM haskell:9.8-bullseye AS build

# Avoid interactive tzdata etc.
ENV DEBIAN_FRONTEND=noninteractive

# Cabal cache priming: copy only files that affect dependency planning
WORKDIR /src
COPY hurve.cabal ./
COPY cabal.project ./

# Update package index & plan deps early for better caching
RUN cabal v2-update

# Pre-build deps (no sources yet -> maximizes cache)
RUN cabal v2-build --only-dependencies

# Now add the actual sources (invalidates cache only when code changes)
COPY . .

# Build & "install" the executable into a clean path
# (Using v2-install to get a single binary we can copy out)
RUN cabal v2-install \
        --installdir=/opt/bin \
        --install-method=copy \
        --overwrite-policy=always

# ---- Runtime ----
FROM debian:bookworm-slim AS runtime

# Create an unprivileged user
RUN useradd -m -U -s /usr/sbin/nologin appuser

# Minimal run deps
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# App directory layout
WORKDIR /app
COPY --from=build /opt/bin/hurve /usr/local/bin/hurve
COPY --from=build /src/static ./static

# (Optional) Healthcheck – simple TCP dial
HEALTHCHECK --interval=30s --timeout=3s --start-period=15s \
    CMD bash -c 'exec 3<>/dev/tcp/127.0.0.1/${PORT:-9160} && exit 0 || exit 1'

# Environment & ports
ENV PORT=9160
EXPOSE 9160

USER appuser

# Use RTS to enable all cores by default (your cabal sets -with-rtsopts=-N too)
CMD ["hurve", "+RTS", "-N"]
