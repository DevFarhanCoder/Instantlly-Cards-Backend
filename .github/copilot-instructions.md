# Copilot Instructions for Instantlly Cards Backend

## Project Overview
This repository powers Instantlly Cards, a backend system with Node.js/TypeScript services and an Erlang/OTP-based WebSocket messaging gateway. It supports scalable messaging, user management, and media handling for mobile/web clients.

## Architecture & Major Components
- **Node.js/TypeScript Backend** (`src/`):
  - REST APIs, authentication, business logic, and database access (MongoDB).
  - Key folders: `models/` (Mongoose schemas), `routes/` (API endpoints), `middleware/` (auth logic), `services/` (business logic), `utils/` (helpers).
- **ErlangMessageGateway/**: Production-grade WebSocket gateway for real-time messaging. Uses OTP supervision tree for reliability and scalability.
- **Uploads/**: Stores user-uploaded media (cards, profiles).

## Developer Workflows
- **Node.js/TypeScript**:
  - Install dependencies: `npm install`
  - Build (if TypeScript): `tsc` (see `tsconfig.json`)
  - Run scripts: `node <script>.js` or `ts-node <script>.ts`
  - Environment config: `.env` (MongoDB, AWS, JWT, SMS keys)
- **Erlang Gateway**:
  - Compile: `./rebar3 compile`
  - Run dev shell: `./rebar3 shell`
  - Run tests: `./rebar3 eunit`
  - Build release: `./rebar3 as prod release`
  - Start production: `./_build/prod/rel/message_gateway/bin/message_gateway.cmd console`

## Key Patterns & Conventions
- **Models**: Defined in `src/models/`, use Mongoose for MongoDB.
- **Routes**: API endpoints in `src/routes/`, grouped by resource.
- **Middleware**: Auth logic in `src/middleware/` (e.g., `adminAuth.ts`, `auth.ts`).
- **Scripts**: One-off admin/data scripts in root and `scripts/`.
- **Erlang Gateway**: Follows OTP supervision tree (see README for details).
- **WebSocket Protocol**: JSON messages for auth, chat, presence, typing, etc. (see Erlang gateway README for schemas).

## Integration Points
- **MongoDB**: Connection string in `.env`.
- **AWS S3/CloudFront**: Media storage, credentials in `.env`.
- **SMS (Fast2SMS)**: API key in `.env`.
- **React Native Client**: Connects to Erlang gateway via WebSocket (see Erlang README for client example).

## Troubleshooting & Tips
- **Port conflicts**: Use `netstat -ano | findstr :8080` and `taskkill /PID <PID> /F` for Erlang gateway.
- **Erlang/Rebar3 issues**: Check installation, update PATH, download rebar3 manually if needed.
- **Health checks**: Erlang gateway at `/health` endpoint.

## Example: Adding a New API Route
1. Create a new file in `src/routes/` (e.g., `myroute.ts`).
2. Define the route handler and import necessary models/services.
3. Register the route in the main Express app (usually in `src/index.ts`).

## Example: Erlang WebSocket Message
```json
{
  "type": "send_message",
  "messageId": "msg_abc123",
  "receiverId": "user456",
  "content": "Hello!",
  "messageType": "text"
}
```

## References
- See `ErlangMessageGateway/README.md` for gateway details, protocol, and architecture.
- See `.env` for integration credentials.
- See `src/` for Node.js backend structure.

---
**For questions or unclear conventions, ask for clarification or check referenced files.**
