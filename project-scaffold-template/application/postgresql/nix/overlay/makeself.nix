self: prev:

# override the makeself package to make sure quiet mode is the default
prev.makeself.overrideAttrs (oldAttrs: {
  fixupPhase = [ oldAttrs.fixupPhase ] ++ [''
    sed -e "s|quiet=\"n\"|quiet=\"y\"|; s|accept=\"n\"|accept=\"y\"|; s|noprogress=\$NOPROGRESS|noprogress=\"y\"|" -i $out/share/makeself-2.4.2/makeself-header.sh
  ''];
})
