@echo off

::
::   ejabberd container installer for Windows
::   -------------------------------------
::                                    v0.4
::
:: This batch script downloads an ejabberd container image
:: and setups a docker container to run ejabberd.

::
:: 1. Download and install Docker:
::
::    If you use Windows 10, download Docker Desktop from:
::      https://www.docker.com/
::
::    If you use Windows 7 or 8, download Docker Toolbox from:
::      https://github.com/docker/toolbox/releases
::    After installation, run Docker Quickstart Installer
::

::
:: 2. Edit those options:

:: Directory where your ejabberd deployment files will be installed
:: (configuration, database, logs, ...)
::
:: In Windows 10 you can configure the path:

set INSTALL_DIR_WINDOWS10=C:\ejabberd

:: In older Windows, not configurable, it will be installed in:
:: C:\Users\%USERNAME%\ejabberd

:: Please enter the desired ejabberd domain name.
:: The domain is the visible attribute that is added to the username
:: to form the Jabber Identifier (for example: user@example.net).
:: This computer must be known on the network with this address name.
:: You can later add more in conf/ejabberd.yml

set HOST=localhost

:: Please enter the administrator username for the current
:: ejabberd installation. A Jabber account with this username
:: will be created and granted administrative privileges.
:: Don't use blankspaces in the username.

set USER=admin

:: Please provide a password for that new administrator account

set PASSWORD=

:: By default this downloads 'latest' ejabberd version,
:: but you can set a specific version, for example '22.05'
:: or the bleeding edge 'master'. See available tags in
:: https://github.com/processone/ejabberd/pkgs/container/ejabberd

set VERSION=latest

:: This tells docker what ports ejabberd will use.
:: You can later configure them in conf/ejabberd.yml

set PORTS=5180 5222 5269 5443

::
:: 3. Now save this script and run it.
::

::
:: 4. When installation is completed:
::
:: If using Windows 10, open Docker Desktop and you can:
::
:: - (>) START the ejabberd container
:: - Enter WebAdmin: click the ([->]) OPEN IN BROWSER button
:: - To try ejabberdctl, click the (>_) CLI button, then: ejabberdctl
:: - ([]) STOP the ejabberd container
::
:: If using an old Windows, open Kitematic and you can:
::
:: - START the ejabberd container
:: - Open your configuration, logs, ... in Settings > Volumes
:: - Enter WebAdmin in Settings > Hostname/Ports > click on the 5180 port
:: - Try ejabberdctl in EXEC, then: ejabberdctl
:: - STOP the ejabberd container
::
:: You can delete the container and create it again running this script,
:: the configuration and database are maintained.
::

::===============================================================
:: Check Windows version
::
::===============================================================

set INSTALL_DIR_DOCKER=c/Users/%USERNAME%/ejabberd

for /f "tokens=4-5 delims=. " %%i in ('ver') do set WVERSION=%%i.%%j
if "%wversion%" == "10.0" (
  echo === Preparing paths to install in Windows 10...
  set INSTALL_DIR=%INSTALL_DIR_WINDOWS10%
  set VC=-v %INSTALL_DIR_WINDOWS10%\conf:/opt/ejabberd/conf
  set VD=-v %INSTALL_DIR_WINDOWS10%\database:/opt/ejabberd/database
  set VL=-v %INSTALL_DIR_WINDOWS10%\logs:/opt/ejabberd/logs
  set VM=-v %INSTALL_DIR_WINDOWS10%\ejabberd-modules:/opt/ejabberd/.ejabberd-modules
  set DOCKERDOWNLOAD="First download and install Docker Desktop from https://www.docker.com/"
) else (
  echo === Preparing paths to install in Windows older than 10...
  set INSTALL_DIR=C:\Users\%USERNAME%\ejabberd
  set VC=-v "/%INSTALL_DIR_DOCKER%/conf:/opt/ejabberd/conf"
  set VD=-v "/%INSTALL_DIR_DOCKER%/database:/opt/ejabberd/database"
  set VL=-v "/%INSTALL_DIR_DOCKER%/logs:/opt/ejabberd/logs"
  set VM=-v "/%INSTALL_DIR_DOCKER%/ejabberd-modules:/opt/ejabberd/.ejabberd-modules"
  set DOCKERDOWNLOAD="First download and install Docker Toolbox from https://github.com/docker/toolbox/releases"
)
set VOLUMES=%VC% %VD% %VL% %VM%

::===============================================================
:: Check docker is installed
::
::===============================================================

docker version >NUL
if %ERRORLEVEL% NEQ 0 (
  echo.
  echo === ERROR: It seems docker is not installed!!!
  echo.
  echo %DOCKERDOWNLOAD%
  echo === Then try to run this script again.
  echo.
  pause
  exit 1
)

::===============================================================
:: Check install options are correctly set
::
::===============================================================

if [%PASSWORD%]==[] (
  echo.
  echo === ERROR: PASSWORD not set!!!
  echo.
  echo === Please edit this script and set the PASSWORD.
  echo === Then try to run this script again.
  echo.
  pause
  exit 1
)

::===============================================================
:: Download Docker image
::
::===============================================================

set IMAGE=ghcr.io/processone/ejabberd:%VERSION%

echo.
echo === Checking if the '%IMAGE%' container image was already downloaded...
docker image history %IMAGE% >NUL
if %ERRORLEVEL% NEQ 0 (
  echo === The '%IMAGE%' container image was not downloaded yet.
  echo.
  echo === Downloading the '%IMAGE%' container image, please wait...
  docker pull %IMAGE%
) else (
  echo === The '%IMAGE%' container image was already downloaded.
)

::===============================================================
:: Create preliminary container
::
::===============================================================

echo.
echo === Checking if the 'ejabberd' container already exists...
docker container logs ejabberd
if %ERRORLEVEL% EQU 0 (
  echo.
  echo === The 'ejabberd' container already exists.
  echo === Nothing to do, so installation finishes now.
  echo === You can go to Docker Desktop and start the 'ejabberd' container.
  echo.
  pause
  exit 1
) else (
  echo === The 'ejabberd' container doesn't yet exist,
  echo === so let's continue the installation process.
)

echo.
if exist %INSTALL_DIR% (
  echo === The INSTALL_DIR %INSTALL_DIR% already exists.
  echo === No need to create the preliminary 'ejabberd-pre' image.
) else (
  echo === The INSTALL_DIR %INSTALL_DIR% doesn't exist.
  echo === Let's create the preliminary 'ejabberd-pre' image.
  CALL :create-ejabberd-pre
)

::===============================================================
:: Create final container
::
::===============================================================

echo.
echo === Creating the final 'ejabberd' container using %IMAGE% image...

setlocal EnableDelayedExpansion
set PS=
for %%a in (%PORTS%) do (
  set PS=!PS! -p %%a:%%a
)

docker create --name ejabberd --hostname localhost %PS% %VOLUMES% %IMAGE%

echo.
echo === Installation completed.
echo.
pause

EXIT /B %ERRORLEVEL%

::===============================================================
:: Function to create preliminary container
::
::===============================================================

:create-ejabberd-pre

echo.
echo === Creating a preliminary 'ejabberd-pre' container using %IMAGE% image...
docker create --name ejabberd-pre --hostname localhost %IMAGE%

echo.
echo === Now 'ejabberd-pre' will be started.
docker container start ejabberd-pre

echo.
echo === Waiting ejabberd to be running...
set /A timeout = 10
set status=4
goto :while

:statusstart
docker exec -it ejabberd-pre ejabberdctl status
goto :statusend

:while
if %status% GTR 0 (
   echo.
   timeout /t 1 /nobreak >NUL
   set /A timeout = timeout - 1
   if %timeout% EQU 0 (
      set status=-1
   ) else (
      goto :statusstart
      :statusend
      set status=%ERRORLEVEL%
   )
   goto :while
)

echo.
echo === Setting a few options...
docker exec -it ejabberd-pre sed -i "s!- localhost!- %HOST%!g" conf/ejabberd.yml
docker exec -it ejabberd-pre sed -i "s!^acl:!acl:\n  admin:\n    user:\n      - \"%USER%@%HOST%\"!g" conf/ejabberd.yml
docker exec -it ejabberd-pre sed -i "s!5280!5180!g" conf/ejabberd.yml
docker exec -it ejabberd-pre sed -i "s!/admin!/!g" conf/ejabberd.yml
docker exec -it ejabberd-pre ejabberdctl reload_config

echo.
echo === Registering the administrator account...
docker exec -it ejabberd-pre ejabberdctl register %USER% %HOST% %PASSWORD%
docker exec -it ejabberd-pre ejabberdctl stop

echo.
echo === Copying conf, database, logs...
mkdir %INSTALL_DIR%
mkdir %INSTALL_DIR%\conf
mkdir %INSTALL_DIR%\database
mkdir %INSTALL_DIR%\logs
mkdir %INSTALL_DIR%\ejabberd-modules
docker cp ejabberd-pre:/opt/ejabberd/conf/ %INSTALL_DIR%
docker cp ejabberd-pre:/opt/ejabberd/database/ %INSTALL_DIR%
docker cp ejabberd-pre:/opt/ejabberd/logs/ %INSTALL_DIR%

echo.
echo === Deleting the preliminary 'ejabberd-pre' container...
docker stop ejabberd-pre
docker rm ejabberd-pre

EXIT /B 0
