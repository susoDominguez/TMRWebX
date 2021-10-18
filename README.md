# CIG Processing Service

#### Deployed at

<img src="Kings_College_London-logo.png" width="150">



<img src="Imperial-College-London-logo.png" width="150">

#### Integrated with

[Heliant]
<img src="heliant_logo.jpeg" width="150">

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Before installing, [download and install Docker](https://www.docker.com/get-started), [Prolog](http://www.swi-prolog.org/Download.html), [python](https://www.python.org/downloads/), [pip](https://packaging.python.org/tutorials/installing-packages/#use-pip-for-installing), [virtualenv](https://virtualenv.pypa.io/en/latest/installation/) and [Node.js](https://nodejs.org/en/download/).

## Running

### Apache Jena Fuseki (Triple Store)
Pull images of busybox and stain/jena-fuseki

```
docker pull busybox
docker pull stain/jena-fuseki
```

Run main container storage:

```
docker run --name fuseki-data -v /fuseki busybox
```

Run main container, supply port (3030, recommended), password,  container store, specify 3 dataset labels (careActions, transitions, beliefs) using TDB version 2, and extend the amount of memory for the Java heap:

```
docker run -e TDB=2 -e FUSEKI_DATASET_1=careActions -e FUSEKI_DATASET_2=transitions -e FUSEKI_DATASET_3=beliefs -e ADMIN_PASSWORD=[pwd] -e JVM_ARGS=-Xmx2g -d --name fuseki -p 3030:3030 --volumes-from fuseki-data stain/jena-fuseki
```

Navigate to http://localhost:3030, login with the username `admin` and the password [pwd] as set above. The three persistent datasets should show. Alternatively, select Manage Datasets, and create the three (persistent) datasets: careActions, transitions and beliefs.

### TMR reasoner

Clone this repository:

```
git clone https://github.kcl.ac.uk/k1214757/TMRWebX.git
```

Change into the backend directory:

```
cd backend
```

Clone the TMR repository:

```
git clone https://github.kcl.ac.uk/k1214757/TMR.git
```

Enter Prolog environment, and include address of Fuseki server as environment variable:

```
FUSEKI_HOST_PORT=http://localhost:3030/ swipl
```

Load server:

```
?- consult('server.pl').
```

Start server on a given port (1234, recommended):

```
?- server(1234).
```

### TMRWeb API

Change to the API folder.

```
cd ../api
```

Create a node virtual environment (within a python virtual environment), and activate it:

```
virtualenv env
. env/bin/activate
pip install nodeenv
nodeenv nenv
. nenv/bin/activate
```

Install dependencies:

```
cat requirements.txt | xargs npm install -g
```

Create an environment file:

```
touch .env
```

Add the following information to this environment file using a text editor:

```
FUSEKI_PASSWORD="[Password]"
JENA_HOST=localhost
PROLOG_HOST=localhost
```

Where [Password] is the password you created for the triple store earlier.

Run server:

```
npm start
```

The server runs by default on port 8888.

## Usage

See [documentation](api/README.md).

## Deployment

Deployment is via [Docker](https://docs.docker.com/compose/install/), and includes containers for this application (api and backend), Fuseki and an optional reverse proxy. If using the reverse proxy, fill in the appropriate [configuration](proxy/nginx.conf).

Build these images:

```
docker-compose build
```

Run these containers:

```
docker-compose up
```

(Optional) Run without proxy:

```
docker-compose up --scale proxy=0
```

Run `setup.py` to create the required Fuseki datasets.

Different docker-compose files exist to accomodate different service configurations.

### Custom certs

To use custom certificates for communication with this service's proxy, reference them in the proxy's [Dockerfile](proxy/Dockerfile). The [gen-domain-cert](proxy/certs/gen-domain-cert.sh) script can be used to generate custom certs (e.g. 'maximoff.crt') using a CA root cert (e.g. 'consult.crt') and accompanying keys. If distributing an image outside of an organisation, edit [Dockerfile](proxy/Dockerfile) and [docker-compose](docker-compose.yml) to mount a volume on the host containing the certs instead, so that images are not transferred with the certs inside then.

## Running the tests

--

## Built With

--

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/martinchapman/nokia-health/tags).

## Authors

Originally produced as part of the [CONSULT project](https://consult.kcl.ac.uk/).
Extended as part of the  [ROAD2H project](https://www.road2h.org/).
![ROAD2H project](http://www.road2h.org/static/images/logo.png "ROAD2H project")

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

* Original source code can be found at [github.com/kclconsult/tmrweb](https://github.com/kclconsult/tmrweb)
* Redesigned processing tool: [github.com/veruskacz/CG-RDF](https://github.com/veruskacz/CG-RDF)
