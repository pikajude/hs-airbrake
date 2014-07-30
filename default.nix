{ cabal, blazeMarkup, exceptions, httpConduit, monadControl
, network, semigroups, text, transformers, utf8String, wai
}:

cabal.mkDerivation (self: {
  pname = "airbrake";
  version = "0.1.0.0";
  sha256 = "cc7d7ad8a58d3d637d732a4c748e1bdfc41ba2308abb31776df7901ece49015e";
  src = ./.;
  buildDepends = [
    blazeMarkup exceptions httpConduit monadControl network semigroups
    text transformers utf8String wai
  ];
  meta = {
    homepage = "https://github.com/joelteon/airbrake";
    description = "An Airbrake notifier for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
