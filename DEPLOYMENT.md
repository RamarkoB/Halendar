# Halendar Deployment Guide

This guide explains how to deploy the Halendar Haskell calendar application to GitHub Pages using Nix and WebAssembly.

## Overview

Halendar is a Haskell web application built with Miso that runs on port 8000. The application uses:

-   GHC 8.10.7
-   Cabal for build management
-   Miso framework for the web interface
-   jsaddle-warp for the web server (development)
-   WebAssembly for production deployment

## GitHub Actions Workflow

The project includes a GitHub Actions workflow that:

1. **Sets up Nix**: Uses the official Nix installer
2. **Configures Cachix**: Uses the haskell-miso-cachix binary cache for faster builds
3. **Creates Nix Flake**: Dynamically creates a Nix flake for building the application
4. **Builds to WebAssembly**: Compiles the Haskell application to WebAssembly using GHC WASM backend
5. **Deploys to GitHub Pages**: Automatically deploys the built application

**Triggers:** Push to main/master branch

## How It Works

### 1. Nix Flake Creation

The workflow creates a `flake.nix` file that:

-   Pins nixpkgs to a stable version
-   Includes the Miso source repository
-   Sets up the WASM32-WASI toolchain
-   Creates a build derivation that compiles your app to WebAssembly

### 2. WebAssembly Compilation

The build process:

-   Creates a WASM-compatible `MainWasm.hs` file with embedded CSS
-   Sets up the proper cabal configuration for WASM compilation
-   Uses `wasm32-wasi-ghc` and `wasm32-wasi-cabal` to compile
-   Generates the necessary JavaScript FFI bindings

### 3. Static Site Generation

The workflow creates:

-   `index.html`: The main HTML file
-   `index.js`: JavaScript loader for the WASM application
-   `halendar.wasm`: The compiled WebAssembly binary
-   `ghc_wasm_jsffi.js`: JavaScript FFI bindings

### 4. GitHub Pages Deployment

Uses the `peaceiris/actions-gh-pages` action to deploy the static files to GitHub Pages.

## Local Development

To run the application locally:

```bash
# Install GHC 8.10.7
ghcup set ghc 8.10.7

# Build and run
cabal run
```

The application will be available at `http://localhost:8000`

## Local WebAssembly Build (Optional)

If you want to test the WebAssembly build locally:

```bash
# Install Nix
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# Enable flakes
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf

# Build the application
nix build

# Serve the result
npx http-server result
```

## Environment Variables

The application uses the following environment variables:

-   `PORT`: The port to run on (default: 8000)

## Troubleshooting

### Build Issues

-   Ensure you're using GHC 8.10.7 as specified in the cabal file
-   Clear cabal cache if needed: `cabal clean`
-   Check that Nix flakes are enabled

### Deployment Issues

-   Check the workflow logs in the Actions tab
-   Verify that GitHub Pages is enabled in your repository settings
-   Ensure the repository has the necessary permissions for GitHub Actions

### WebAssembly Issues

-   The WASM build requires a modern browser with WebAssembly support
-   Check browser console for any JavaScript errors
-   Verify that all required files are present in the deployment

## Customization

### Modifying the Build

-   Edit the `flake.nix` content in the workflow to change build configuration
-   Modify the embedded CSS in `MainWasm.hs` to change styling
-   Update the cabal dependencies in the generated cabal file

### Adding Features

-   The WASM version embeds CSS directly in the Haskell code
-   Any changes to the main application logic should work in both development and WASM modes
-   Consider the differences between server-side (jsaddle-warp) and client-side (WASM) execution

## Security Notes

-   The WebAssembly build runs entirely in the browser
-   No server-side code is deployed to GitHub Pages
-   All data is stored locally in the browser
-   No API keys or sensitive information are exposed

## Performance

-   WebAssembly provides near-native performance
-   The application loads as a single-page application
-   CSS is embedded to reduce HTTP requests
-   The binary cache ensures fast builds in CI/CD

## Browser Support

The WebAssembly build requires:

-   Modern browsers with WebAssembly support
-   ES6 module support
-   Fetch API support

Supported browsers:

-   Chrome 57+
-   Firefox 52+
-   Safari 11+
-   Edge 79+
