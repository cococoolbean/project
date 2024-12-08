# SIMP Compiler Web Application
This is a web-based frontend for the SIMP compiler that allows users to:

    1.Write SIMP code in a text area.
    2.Compile it to WebAssembly (WASM).
    3.Run the compiled WASM code.

### Running the Application
Build the Application
```bash
stack build . yesod-bin
```
Run the Development Server
```bash
stack exec yesod devel
```
Open your browser and navigate to:
http://localhost:3000

## Customizing the Filepath for the SIMP Binary
The application uses a relative or absolute path to the simp binary to execute the compilation. By default, the filepath is:

```haskell
let command = currentDir </> ".stack-work/install/x86_64-linux/491227709979cde0824fabc19671f319aa9cd80bfe86e141d5356773d292d7fd/9.2.8/bin/simp"
```

### Changing the Filepath
If the binary is located in a different directory, you must update the filepath in the following file:

File: src/Handler/Compile.hs
Line:
```haskell
let command = currentDir </> ".stack-work/install/x86_64-linux/491227709979cde0824fabc19671f319aa9cd80bfe86e141d5356773d292d7fd/9.2.8/bin/simp"
```
Replace the path with the correct one for your system. 
