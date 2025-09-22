## Tiltfile for TMRWebX
##
## Maintainer: Jesus Dominguez <k1214757@kcl.ac.uk>
# NOTE: Ensure you are running Tilt version 0.32.0 or later.

# Read local configuration
cfg = read_json('tiltconf.json')
print("Configuration loaded: " + cfg.get('name'))

# Load Tilt extensions
load('ext://uibutton', 'cmd_button', 'location', 'text_input')
load('ext://dotenv', 'dotenv')

# Load environment variables
dotenv('./api/.env')

# -------------------
# Development Mode Configuration
# -------------------

# Check if we're in development mode
dev_mode = cfg.get('dev_mode', True)
print("Development mode: " + str(dev_mode))

# -------------------
# Build API Service with Live Updates
# -------------------

if dev_mode:
    # Development build with live reload
    docker_build(
        ref='road2h-interaction_ms',
        context='./api',
        dockerfile='./api/Dockerfile',
        build_args={
            'NODE_ENV': 'development', 
            'SKIP_COPY': 'false'
        },
        ignore=[
            '.dockerignore',
            'node_modules', 
            '.git',
            'app.log',
            '*.log',
            '.nyc_output',
            'coverage',
            '.vscode',
            '.DS_Store'
        ],
        live_update=[
            # Sync source code changes
            sync('./api/', '/usr/src/app/'),
            # Handle dependency changes
            sync('./api/package.json', '/usr/src/app/package.json'),
            sync('./api/package-lock.json', '/usr/src/app/package-lock.json'),
            # Reinstall dependencies when package files change
            run('npm install', trigger=['./api/package.json', './api/package-lock.json']),
        ]
    )
else:
    # Production build
    docker_build(
        ref='road2h-interaction_ms',
        context='./api',
        dockerfile='./api/Dockerfile',
        build_args={'NODE_ENV': 'production'}
    )

# -------------------
# Build Prolog Reasoner Service
# -------------------

docker_build(
    ref='road2h-reasoner_ms',
    context='./backend',
    dockerfile='./backend/Dockerfile',
    ignore=['.git', '*.log'],
    live_update=[
        sync('./backend/', '/usr/server/backend/'),
    ] if dev_mode else []
)

# -------------------
# Docker Compose Services
# -------------------

# Use docker-compose for orchestration
docker_compose('docker-compose.yml')

# -------------------
# Resource Configuration
# -------------------

# Configure the store (Fuseki) service
dc_resource(
    'store_app',
    labels=['database']
)

# Configure the reasoner (Prolog) service  
dc_resource(
    'reasoner_app',
    labels=['backend']
)

# Configure the API service with debugging port
dc_resource(
    'interaction_app',
    labels=['api', 'frontend']
)

# -------------------
# Utility Functions and Commands
# -------------------

# API restart command for quick restarts
def api_restart():
    return 'docker-compose restart interaction_app'

# Database initialization script
exec_fuseki_init = '''
set -eu
echo "Waiting for Fuseki to be ready..."
until curl -sf http://localhost:3030/$/ping; do
  echo "Fuseki is not ready yet..."
  sleep 2
done

echo "Fuseki is ready. Initializing datasets..."
curl -X POST --data "dbType=tdb2" --data "dbName=careActions" http://localhost:3030/$/datasets 2>/dev/null || echo "careActions dataset exists"
curl -X POST --data "dbType=tdb2" --data "dbName=transitions" http://localhost:3030/$/datasets 2>/dev/null || echo "transitions dataset exists"  
curl -X POST --data "dbType=tdb2" --data "dbName=beliefs" http://localhost:3030/$/datasets 2>/dev/null || echo "beliefs dataset exists"
curl -X POST --data "dbType=tdb2" --data "dbName=statements" http://localhost:3030/$/datasets 2>/dev/null || echo "statements dataset exists"
curl -X POST --data "dbType=tdb2" --data "dbName=guidelines" http://localhost:3030/$/datasets 2>/dev/null || echo "guidelines dataset exists"

echo "Fuseki dataset initialization completed!"
'''

# -------------------
# UI Buttons for Development
# -------------------

cmd_button(
    name='restart-api',
    text='🔄 Restart API',
    resource='interaction_app',
    argv=['sh', '-c', api_restart()],
    icon_name='refresh',
)

cmd_button(
    name='init-fuseki',
    text='🗄️ Init Fuseki Datasets',
    resource='store_app', 
    argv=['sh', '-c', exec_fuseki_init],
    icon_name='database',
)

cmd_button(
    name='view-logs-api',
    text='📋 View API Logs',
    resource='interaction_app',
    argv=['docker-compose', 'logs', '-f', 'interaction_app'],
    icon_name='description',
)

cmd_button(
    name='view-logs-reasoner',
    text='📋 View Reasoner Logs', 
    resource='reasoner_app',
    argv=['docker-compose', 'logs', '-f', 'reasoner_app'],
    icon_name='description',
)

# -------------------
# Local Resources for Initialization
# -------------------

# Automatic Fuseki initialization
local_resource(
    'fuseki-init',
    cmd=exec_fuseki_init,
    resource_deps=['store_app'],
    labels=['setup']
)

# -------------------
# Health Checks and Monitoring
# -------------------

# API health check
local_resource(
    'api-health-check',
    cmd='curl -f http://localhost:8888/health || exit 1',
    resource_deps=['interaction_app'],
    labels=['monitoring']
)

print(" TMRWebX development environment configured!")
print(" Available services:")
print("   - API: http://localhost:8888") 
print("   - Fuseki: http://localhost:3030")
print("   - Prolog Reasoner: http://localhost:1234")
print("   - Debug Port: localhost:9229")
print(" Use the UI buttons for quick actions!")