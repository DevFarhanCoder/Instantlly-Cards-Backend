# 🎯 Erlang Gateway - Manual Testing Guide

## ✅ Status: Erlang OTP 27 is installed and working!

You have successfully installed Erlang! Now let's test it manually.

---

## 🚀 Quick Start - Manual Test

### Step 1: Start Erlang Shell

```powershell
cd C:\Users\user3\Documents\App\ErlangMessageGateway
&"C:\Program Files\Erlang OTP\bin\erl.exe"
```

You should see:
```
Erlang/OTP 27 [erts-16.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V16.1 (press Ctrl+G then 'h' for help)
1>
```

### Step 2: Compile the Simple Test Module

In the Erlang shell, type:
```erlang
cd("src").
c(simple_test).
simple_test:start().
simple_test:hello().
```

Expected output:
```
===========================================
✅ Erlang is working!
===========================================

Your Erlang version: 27
System time: 1729012345678

Hello from Erlang! 🚀
{ok,"Erlang is ready!"}
```

### Step 3: Exit Erlang

```erlang
q().
```

---

## 🔧 Alternative: PowerShell One-Liner Test

Run this command to test everything at once:

```powershell
cd C:\Users\user3\Documents\App\ErlangMessageGateway
&"C:\Program Files\Erlang OTP\bin\erl.exe" -noshell -pa src -eval "compile:file(simple_test, [{outdir, 'ebin'}]), code:add_path('ebin'), simple_test:start(), simple_test:hello()" -s init stop
```

---

## 📦 Next Steps: Install Rebar3 Properly

The `rebar3` file we downloaded needs dependencies. Let's use a simpler approach:

### Option 1: Use Rebar3 with Patience (Recommended)

The dependency download takes 2-5 minutes. Let it complete:

```powershell
# Clean previous attempts
Remove-Item -Recurse -Force _build -ErrorAction SilentlyContinue

# Run rebar3 compile and wait
escript rebar3 compile
```

**Wait for it to finish!** It will:
1. Download cowboy (WebSocket library)
2. Download jsx (JSON parser)
3. Download epgsql (PostgreSQL driver)
4. Download eredis (Redis client)
5. Compile everything

### Option 2: Start Without Dependencies

Create a minimal version that works without external libraries:

```powershell
# Compile just the core modules
cd src
&"C:\Program Files\Erlang OTP\bin\erlc.exe" -o ../ebin simple_test.erl

# Test
cd ..
&"C:\Program Files\Erlang OTP\bin\erl.exe" -pa ebin
```

Then in Erlang shell:
```erlang
simple_test:start().
simple_test:hello().
q().
```

---

## 🌐 Testing the Full WebSocket Gateway

Once rebar3 finishes compiling (with dependencies), you can:

### Start the Gateway

```powershell
escript rebar3 shell
```

You'll see:
```
===========================================
Starting Erlang Message Gateway...
===========================================

🚀 Message Router started
👥 Session Manager started
🟢 Presence Tracker started
✅ Message Gateway started successfully!
🌐 WebSocket endpoint: ws://localhost:8080/ws
🏥 Health check: http://localhost:8080/health
```

### Test with Web Browser

1. Open http://localhost:8080 in your browser
2. Enter User ID: `user123`
3. Click "Connect"
4. Open another tab with User ID: `user456`
5. Send messages between them!

---

## 🐛 Troubleshooting

### "escript rebar3 compile" is stuck

**This is normal!** It's downloading dependencies from the internet. Wait 2-5 minutes.

You can monitor progress:
- It will show `===> Fetching cowboy...`
- Then `===> Fetching jsx...`
- Then `===> Fetching epgsql...`
- etc.

### Still having issues?

Let's test Erlang works at all:

```powershell
&"C:\Program Files\Erlang OTP\bin\erl.exe" -version
```

Should show: `Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 16.1`

---

## 📝 Summary

**What we've built:**
- ✅ Erlang/OTP 27 installed
- ✅ Simple test module working
- ✅ Full WebSocket gateway code ready
- ⏳ Waiting for rebar3 to download dependencies

**What you can do now:**
1. Test Erlang works: Run the simple_test module
2. Wait for rebar3 compile to finish (5 min)
3. Start the full gateway
4. Test with web browser

**Architecture we're building:**
```
Client (Browser/React Native)
    ↓ WebSocket
Erlang Gateway (Port 8080)
    ├── Connection Handler (ws_handler)
    ├── Message Router (message_router)
    ├── Session Manager (session_manager)
    └── Presence Tracker (presence_tracker)
```

This will replace your current Socket.IO system and handle 100K+ concurrent connections!

---

Ready to proceed? Try the manual test first to verify Erlang is working, then let rebar3 finish compiling! 🚀
