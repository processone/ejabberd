
@if "x%1"=="x--help" goto usage

@set arg=dynamic
@if "x%1"=="x--static" set arg=static

@echo Configuring for %arg% build...

erlc configure.erl
erl -s configure -env arg %arg% -noshell

@goto end

:usage
@echo Usage: configure.bat
@echo or configure.bat --static
@echo or configure.bat --help

:end

