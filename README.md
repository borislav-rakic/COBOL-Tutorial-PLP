# Setting up COBOL
This document will explain how to install the GnuCOBOL compiler on macOS, Linux, and Windows, and how to set up Visual Studio Code for COBOL coding.

## emp_file.dat
Make sure to download this file and put it right next to your COBOL program during the tutorial (if you decide to code along).

## macOS
Make sure you have a C compiler installed by running the following command:

```
gcc --version
```

If you don’t have a C compiler installed, run the following command to install:

```
xcode-select --install
```

The simplest way of installing GnuCOBOL on macOS is through homebrew. If you do not have homebrew installed, you have two options:

1.	<b>(Recommended)</b> Install homebrew https://brew.sh, then simply install GnuCOBOL using the command “brew install gnucobol”
2.	Compile GnuCOBOL yourself without homebrew https://gnucobol.sourceforge.io

If you decide to go with option 2, there are instructions on how to compile GnuCOBOL in the README file, when you click on the button saying “MacOS”.

## Linux
Make sure you have a C compiler installed by running the following command:

```
gcc --version
```

If you don’t have a C compiler installed, run the following command(s) to install:

<b>Debian/Ubuntu:</b>

```
sudo apt update	
sudo apt install build-essential
```

<b>Fedora/Red Hat:</b>

```
sudo dnf groupinstall "Development Tools"
```

The simplest way of installing GnuCOBOL on macOS is through homebrew. If you do not have homebrew installed, you have two options:

1.	<b>(Recommended)</b> Install homebrew https://brew.sh, then simply install GnuCOBOL using the command “brew install gnucobol”
2.	Compile GnuCOBOL yourself without homebrew https://gnucobol.sourceforge.io

If you decide to go with option 2, there are instructions on how to compile GnuCOBOL in the README file, when you click on the button saying “Linux”.

## Windows
To install GnuCOBOL on Windows, you also have two options:

1. <b>(Recommended)</b> Install a prebuilt binary made by Arnold Trembley https://www.arnoldtrembley.com/GnuCOBOL.htm
    1. Scroll down, until you find the header “(UPDATED 2023-07-30) MSYS2 64-bit MinGW GnuCOBOL 3.2 for Windows”
    2. Download GC32M-BDB-x64.7z
    3. Extract the archive to a folder `C:\\GnuCOBOL`
    4. Read through the file `STARTHERE.txt`
    5. Once you run `set_env.cmd`, you need to keep that terminal open and navigate to the folder containing your COBOL code (in VS Code, you can open the folder your COBOL code is located in, and then simply drag the `set_env.cmd` file from the folder `C:\\GnuCOBOL` into the terminal)
    6. You are then able to compile your COBOL code using `cobc -x file.cbl` and run it using `file.exe`
2.	Compile GnuCOBOL yourself https://gnucobol.sourceforge.io

If you decide to go with option 2, there are instructions on how to compile GnuCOBOL in the README file, when you click on the button saying “Windows”.

## Visual Studio Code
In order to be able to program efficiently, I recommend using Visual Studio Code, along with a COBOL extension. I recommend the following COBOL extension: https://marketplace.visualstudio.com/items?itemName=bitlang.cobol

Once you installed the extension, you’re good to go. Let’s try it out by creating a simple “Hello World!” program:
Create a file called `hello_world.cbl`.
 
A simple “Hello World!” program in COBOL looks like this:

```COBOL
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

PROCEDURE DIVISION.
DISPLAY "Hello World!".

STOP RUN.
```
 
To run the program, you first need to compile it. You can do that through the command line with the following command:

```
cobc -x hello_world.cbl
```

Afterwards, a file called `hello_world`/`hello_world.exe` will be created. You can execute the program in the command line like this:

<b>macOS/Linux:</b> 

```
./hello_world
```

<b>Windows:</b> 

```
hello_world.exe
```
