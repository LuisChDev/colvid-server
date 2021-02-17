# colvid-server

Este repositorio implementa un servidor sencillo que, a través de una base de
datos SQLite, expone tres APIs, para obtener usuarios y películas por ID, y
para obtener archivos estáticos.

## Instrucciones

- Descarga el repositorio:

``` bash
$ git clone https://github.com/LuisChDev/colvid-server
```

- instala **Nix** en tu sistema:

``` bash
$ curl -L https://nixos.org/nix/install | sh
```

(requiere acceso a sudo)

- compila el proyecto con el siguiente comando:

``` bash
$ nix-build release.nix
```

- para correr el proyecto en desarrollo, quizá sea mejor hacerlo
  desde Haskell interactivo:

``` bash
$ cd src
$ ghci -F -pgmF=record-dot-preprocessor
GHCi, version 8.10.3: https://www.haskell.org/ghc/  :? for help
Prelude> :l Server
[1 of 2] Compiling Internal.Types   ( Internal/Types.hs, interpreted )
[2 of 2] Compiling Server           ( Server.hs, interpreted )
Ok, two modules loaded.
*Server> colvidServer
```

## API

el servidor permite agregar nuevos usuarios mediante un llamado POST a la 
dirección `/users`:

``` bash
curl -i -X POST -H 'Content-Type: application/json' -d '{"name": "Carlos", "idn": "1", "email": "carlos@empresa.com", "registrationDay": "2010-06-20"}'  http://localhost:8081/users
```

asimismo, es posible obtener los datos de un usuario mediante un llamado GET.

``` bash
curl -i -X GET http://localhost:8081/users?id=1
```

