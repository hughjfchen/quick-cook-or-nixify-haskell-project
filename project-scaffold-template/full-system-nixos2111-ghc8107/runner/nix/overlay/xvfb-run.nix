self: prev:
prev.xvfb-run.overrideAttrs (old: {
  buildCommand = old.buildCommand + ''
    sed -i '/kill $XVFBPID/ s:^:[ "X$(ps -eo pid|grep $XVFBPID|grep -v grep)" != "X" ] \&\& :g' $out/bin/.xvfb-run-wrapped '';
})
