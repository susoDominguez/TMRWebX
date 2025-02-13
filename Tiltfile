# Define services for interaction-app
docker_build(
    'road2h-interaction_ms',  # Specify the image name
    './api',  # Specify the build context (directory)
    dockerfile='./api/Dockerfile',  # Dockerfile location
    build_args={'NODE_ENV': 'development', 'SKIP_COPY': 'true'},
    live_update=[
        sync('./api/', '/usr/src/app/'),
        run('npm install', trigger=['./api/package.json', './api/package-lock.json']),
        run('npm run devstart', trigger=['./api/**/*.js', './api/**/*.ts'])
    ]
)

# Define services for reasoner-app
docker_build(
    'road2h-reasoner_ms',  # Specify the image name
    './backend',  # Specify the build context (directory)
    dockerfile='./backend/Dockerfile',  # Dockerfile location
    build_args={'NODE_ENV': 'development', 'buildtime_FUSEKI_HOST': 'store_service', 'buildtime_FUSEKI_PORT': '3030'},
    live_update=[
        sync('./backend/', '/usr/server/backend/'),
        run('apt-get update && apt-get install -y build-essential && apt-get clean && rm -rf /var/lib/apt/lists/*', trigger=['./backend/requirements.txt']),
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
k8s_resource('interaction-app', port_forwards=8888, auto_init=True)
k8s_resource('reasoner-app', port_forwards=1234, auto_init=True)
k8s_resource('store-app', port_forwards=3030, auto_init=True)