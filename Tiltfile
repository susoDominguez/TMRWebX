## Tiltfile for TMRWebX
##
## Maintainer: Jesus Dominguez <k1214757@kcl.ac.uk>
# NOTE: Ensure you are running Tilt version 0.32.0 or later.

# Read local configuration
cfg = read_json('tiltconf.json')
print("Configuration loaded: " + cfg.get('name'))

# Load all K8s manifests
k8s_yaml([
"./chart/"+cfg.get('name')+"/reasoner-service.yaml",
"./chart/"+cfg.get('name')+"/reasoner-deployment.yaml",
"./chart/"+cfg.get('name')+"/interaction-service.yaml",
"./chart/"+cfg.get('name')+"/interaction-deployment.yaml",
"./chart/"+cfg.get('name')+"/store-service.yaml",
"./chart/"+cfg.get('name')+"/store-deployment.yaml"
])

# Load Tilt extensions
load('ext://uibutton', 'cmd_button', 'location', 'text_input')
load('ext://dotenv', 'dotenv')

# Load environment variables
dotenv('./api/.env')


# -------------------
# Build Image
# -------------------

docker_build(
    ref='road2h-interaction_ms',
    context='./api',
    build_args={'NODE_ENV': 'development', "VERSION": "1.0.0"},
    ignore=['.dockerignore',"node_modules", ".git","app.log"],
    live_update=[
        sync('./api/', '/usr/src/app/'), # Sync the local directory to the container
        sync('./api/package.json', '/usr/src/app/package.json'),
        sync('./api/package-lock.json', '/usr/src/app/package-lock.json'),
        run('npm install', trigger=['./api/package.json', './api/package-lock.json']),
        run('npx nodemon --inspect=0.0.0.0:9229 ./bin/www')
    ]
)

# -------------------
# Define Kubernetes Resources
# -------------------

k8s_resource('interaction-app', port_forwards=['8888:8888', '9229:9229'], depends_on=['store-app', 'reasoner-app'])

#######################

docker_build(
    'road2h-reasoner_ms',
    context='./backend',
    dockerfile='./backend/Dockerfile',
    live_update=[
        sync('./backend/', '/usr/server/backend/'),
        run('swipl -l server.pl', trigger=['./backend/server.pl']),
        run('swipl -g server(1234)')
    ]
)


k8s_resource('store-app', port_forwards=['3030:3030'])
k8s_resource('reasoner-app', port_forwards=['1234:1234'], depends_on=['store-app'])


# -------------------
# Initialize Fuseki Datasets
# -------------------

exec_fuseki_init = '''
set -eu
POD_NAME="$(kubectl get pod -l app=store-app -o jsonpath='{.items[0].metadata.name}')"
echo "$POD_NAME"

# Wait for Fuseki to be fully available
echo "Waiting for Fuseki to be ready..."
until kubectl exec "$POD_NAME" -- curl -sf http://localhost:3030/; do
  echo "Fuseki is not ready yet..."
  sleep 5
done

echo "Fuseki is ready. Initializing datasets..."
kubectl exec "$POD_NAME" -- curl -X POST --data "dbType=tdb2" http://localhost:3030/careActions
kubectl exec "$POD_NAME" -- curl -X POST --data "dbType=tdb2" http://localhost:3030/transitions
kubectl exec "$POD_NAME" -- curl -X POST --data "dbType=tdb2" http://localhost:3030/beliefs
kubectl exec "$POD_NAME" -- curl -X POST --data "dbType=tdb2" http://localhost:3030/statements

echo "Fuseki dataset initialization completed!"
'''

cmd_button(
    name='fuseki-init',
    text='Init Fuseki Datasets',
    resource='store-app',
    argv=['sh', '-c', exec_fuseki_init],
    icon_name='database',
)

local_resource('fuseki_init', cmd=exec_fuseki_init, resource_deps=['store-app'])

docker_compose('docker-compose.yml')