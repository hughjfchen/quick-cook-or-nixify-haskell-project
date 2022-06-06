self: prev: {
  # sample pakage override
  # xxxx = prev.xxxx.overrideAttrs (oldAttr: rec { buildInputs = prev.xxxx.buildInputs ++ [ self.openssl ]; });
}
