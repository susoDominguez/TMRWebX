# Tiltfile for deploying services locally with Tilt

# Define services for interaction-app
docker_build(
    'road2h-interaction_ms',
    './api',
    dockerfile='./api/Dockerfile',
    build_args={'NODE_ENV': 'development', 'SKIP_COPY': 'true'},
    live_update=[
        # Sync the application code (except node_modules) into the container
        sync('./api/', '/usr/src/app/'),
        # Trigger npm install only if package.json or package-lock.json changes
        run('npm install', trigger=['./api/package.json', './api/package-lock.json']),
        # Optionally, restart the app when the source code changes (ensure live updates happen seamlessly)
        run('npm run devstart', trigger=['./api/**/*.js', './api/**/*.ts'])
    ]
)

# Define services for reasoner-app
docker_build(
    'road2h-reasoner_ms',
    './backend',
    dockerfile='./backend/Dockerfile',
    build_args={'NODE_ENV': 'development', 'buildtime_FUSEKI_HOST': 'store_service', 'buildtime_FUSEKI_PORT': '3030'},
    live_update=[
        # Sync the backend directory into the container
        sync('./backend/', '/usr/server/backend/'),
        # Run Prolog dependencies installation if necessary (e.g., after changes to requirements files)
        run('apt-get update && apt-get install -y build-essential && apt-get clean && rm -rf /var/lib/apt/lists/*', trigger=['./backend/requirements.txt']),
        # Optionally, restart the Prolog server if needed
        run('kill -HUP $(cat /tmp/prolog.pid)', trigger=['./backend/server.pl'])
    ]
)

# Load Kubernetes manifests
k8s_yaml([
    'k8s/persistent-volume.yaml',
    'k8s/store-app.yaml',
    'k8s/interaction-app.yaml',
    'k8s/reasoner-app.yaml'
])

# Define services' port forwarding for development access
k8s_resource('store-app', port_forwards=3030, auto_init=True)
k8s_resource('interaction-app', port_forwards=8888)
k8s_resource('reasoner-app', port_forwards=1234)