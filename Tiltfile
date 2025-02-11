# Tiltfile for deploying services locally with Tilt

# Define services
docker_build('road2h-interaction_ms', './api', dockerfile='./api/Dockerfile', build_args={'NODE_ENV': 'development', 'SKIP_COPY': 'true'})
docker_build('road2h-reasoner_ms', './backend', dockerfile='./backend/Dockerfile')

# Define Kubernetes namespace
k8s_yaml([
    'k8s/persistent-volume.yaml',
    'k8s/store-app.yaml',
    'k8s/interaction-app.yaml',
    'k8s/reasoner-app.yaml'
])

# Define services' port forwarding for development access
k8s_resource('store-app', port_forwards=3030)
k8s_resource('interaction-app', port_forwards=8888)
k8s_resource('reasoner-app', port_forwards=1234)
