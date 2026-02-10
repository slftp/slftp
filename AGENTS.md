# Development Configuration

## cbftp Connection

```ini
[UDPConfig]
EnableUDP=True
IP=127.0.0.1
Port=5696
ApiPort=55477
Password=testrace!
```

- cbftp data directory: `/root/.cbftp/data`

## slftp API

- API endpoint: `http://localhost:8089/api`
- API key location: `config/slftp.ini` -> `[api]` -> `apikey=...`
