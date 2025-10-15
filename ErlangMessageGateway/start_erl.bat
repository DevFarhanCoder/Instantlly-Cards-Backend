@echo off
echo ==========================================
echo Starting Erlang Shell
echo ==========================================
echo.
echo Commands you can try:
echo   c(simple_test).           - Compile simple_test module
echo   simple_test:start().      - Run start function
echo   simple_test:hello().      - Run hello function
echo   q().                      - Quit
echo.
"C:\Program Files\Erlang OTP\bin\erl.exe" -pa ebin
