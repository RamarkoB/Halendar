{
  description = "Halendar - Haskell Calendar built with Miso";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    miso.url = "github:dmjio/miso";
    miso.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, miso }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system} = {
        default = pkgs.stdenv.mkDerivation {
          name = "halendar";
          src = ./.;

          nativeBuildInputs = with pkgs; [
            nodejs_20
            git
            cacert
            # Use miso's development environment
            miso.packages.${system}.default
          ];

          buildPhase = ''
            # Set up proper environment
            export HOME=$TMPDIR
            export CABAL_DIR=$TMPDIR/.cabal
            mkdir -p $CABAL_DIR
            
            # Set up SSL certificates for HTTPS access
            export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
            export NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
            
            # Configure cabal to use local package database
            echo "repository hackage.haskell.org" > $CABAL_DIR/config
            echo "  url: https://hackage.haskell.org/" >> $CABAL_DIR/config
            echo "  secure: True" >> $CABAL_DIR/config
            echo "  root-keys: 07c59cb65787dedfaef5bd5f987ceb5f7e5ebf88b904bbd4c5cbdeb2ff71b740" >> $CABAL_DIR/config
            echo "  key-threshold: 3" >> $CABAL_DIR/config
            
            # Build the Miso application for WASM
            echo "Building Halendar with Miso for WASM deployment..."
            
            # Use miso's build system to create the web assets
            miso build calApp --target=wasm --one-shot
            
            # The build should create dist-* directories with the web assets
            echo "Build completed. Looking for output files..."
            ls -la
            find . -name "*.js" -o -name "*.html" -o -name "*.wasm" -o -name "*.css" | head -20
          '';

          installPhase = ''
            # Create output directory
            mkdir -p $out
            
            # Copy the built web assets
            if [ -d "dist-newstyle" ]; then
              echo "Found dist-newstyle directory"
              find dist-newstyle -name "*.js" -o -name "*.html" -o -name "*.wasm" -o -name "*.css" -exec cp {} $out/ \;
            fi
            
            # Copy the CSS file
            cp cal.css $out/
            
            # Create index.html if it doesn't exist
            if [ ! -f "$out/index.html" ]; then
              cat > $out/index.html << 'HTML_EOF'
            <!DOCTYPE html>
            <html lang="en">
            <head>
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <title>Halendar - Haskell Calendar</title>
                <link rel="stylesheet" href="cal.css">
            </head>
            <body>
                <div id="app">
                    <h1>Loading Halendar...</h1>
                </div>
                <script src="all.js" type="module"></script>
            </body>
            </html>
            HTML_EOF
            fi
            
            echo "Installation completed. Files in $out:"
            ls -la $out
          '';

          # Add some debugging
          dontStrip = true;
          enableParallelBuilding = false;
        };
      };
    };
} 