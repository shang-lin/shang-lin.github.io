Interpreted languages like Python and Perl have become ubiquitous. It is easier and faster to code in Python than a lower-level language like C, but skipping the compile step also means it\'s harder to catch syntax or type errors before you run your code. 

Luckily, there are ways to check your interpreted code for errors before running. One way is to use an Integrated Development Environment (IDE), such as PyCharm for Python, or a vim plug-in like Syntastic. PyCharm detects errors as you code. Syntastic checks syntax for a variety of languages when you save your file.

You can also check your code from the command line. I will show you how to do this in three popular interpreted languages: Python, Perl, and bash.

**Python**

Although Python is an interpreted language, it is  compiled to bytecode that is then executed by a VM. You might have seen the bytecode as ``.pyc`` files.

To compile Python into bytecode without executing, run ``python`` with the ``py_compile`` module:

    python -m py_compile myprogram.py
    
If ``myprogram.py`` contains errors other than logical mistakes, you will catch them in this step.

Compiling is not your only option. The ``pylint`` tool checks syntax, style, and more. To install and use``pylint``, run:

    pip install pylint
    pylint myprogram.py
    
**Perl**

Syntax checking Perl is simple. Run the Perl interpreter on your program with the ``-c`` flag:

    perl -c myprogram.pl
    
to check your syntax without running the program.

**Bash**

Bash is not a full-featured programming language like Python or Perl, but it is interpreted and error-prone. Call bash with ``-n`` flag on your script to check its syntax without running it:

    bash -n mybashscript.bash

