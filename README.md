#  R package cxapp

cxapp is a collection of R utilities, functions and objects for developing R 
based Shiny, plumber and other applications and services in Life Science GxP
and regulated environments.

This is an initial development release to explore the functionality across 
several different platforms and integrated within compute environments.

This release of the cxapp package includes a simple configuration management 
based on property files, logging and managing secrets through vaults. The 
package also includes a simple vault using the local file system.

Future version will incorporate authentication methods, session managements, 
session and application cache, regulatory compliant audit trail, using remote
services and much more.

<br/>


## Getting Started

Download and install the latest release of cxapp from https://github.com/cxlib/r-package-cxapp/releases/latest

You can also install the latest release directly using `install.packages()`.   

```
install.packages( "https://github.com/cxlib/r-package-cxapp/releases/download/v0.5.0/cxapp_0.5.0.tar.gz", type = "source", INSTALL_opts = "--install-tests" )
```

To install prior releases, replace the version number references in the URL.

<br/>



## Configuration Properties

The cxapp package contains a simple approach to app and service configuration using
the R reference class `cxapp_config( <paths> )`. 

The `cxapp_config()` function first searches for and uses the first occurrence of
the file `cxapp.properties` in a cxapp sub-directory under `.libPaths()`, as would
be the case if cxapp is installed and has a properties file. The function then
searches for property files in the specified sequence of paths. If a path is 
specified as a directory, all property files within the directory are processed
in natural sort order.

The property file name consists of one or more of the characters A-Z and digits 0-9
and ends in the file extension `.properties`.

The property file name, excluding the file extension, defines the property context
in references using the convention `<context>/<property name>`. The default context
is `cxapp` and can be omitted.

A property is defined as a key/value pair. The key consists of one or more of the 
characters A-Z, digits 0-9 and punctuations underscore `_` and period `.`. 

The first equal sign `=` or a colon `:` separates the key from value. Any additional
delimiters are assumed part of the value. Leading and trailing white space is removed 
from the value. 

Lines in the property file starting with a hash `#` or exclamation point `!` are 
treated as comments. Empty lines are ignored.

A property value prefixed with the tag `[env]` or starting with the character `$`
refers to an environmental variable. The value of the environmental variable is 
returned as the value of the property.

A property value prefixed with the tag `[vault]` refers to the name of a secret 
stored in a connected vault (see Vaults below). The value of the vault secret 
is returned as the value of the property.

See help for `cxapp::cxapp_config()` for further details.

<br/>

## Logs
The cxapp package includes a simple logging utility.

The log directory is specified by the cxapp property `LOG.PATH`.

The root of the log file name can be customized using the cxapp property
`LOG.NAME`. If the log name is not specified, the default name is `app.log`.

A simple form of automated rotation of logs can defined using the `LOG.ROTATION`
cxapp property. A property value of 
* `YEAR` enables annual rotation and results in the log file name `<name>-yyyy.log`
* `MONTH` enables monthly rotation and results in the log file name `<name>-yyyymm.log`
* `DAY` enables daily rotation and results in the log file name `<name>-yyyymmdd.log`

Log entries in the log file are prefixed by the date and time of when the log entry was
created/appended. 

All log references to date and time is in UTC format.

See help for `cxapp::cxapp_log()` for further details.

<br/>

## App Paths
The cxapp package provides simple functions for app paths.

The `cxapp_datapath()` function provides a simple means to refer to sub-directories
within the app or service data directory, similar to the conventions of
`base::file.path()`.

The app or service data directory is defined with the cxapp property `DATA`. The 
property value can be a delimited list of paths. The first existing is used as the
root of the sub-directories. If none of the paths in the property exist, the first
path in the list is used.

See help for `cxapp::cxapp_dataPath()` for further details.

<br/>

## Vaults
The package supports using key vaults to store and retrieve secrets. Currently, only
Azure Key Vault and a local simple file system implementation is supported.

The package function `cxapp_vault()` returns a connection object to the configured
vault. Currently, only one vault service can be configured at the same time.

_It is on the role of the developer and implementer to ensure 
that the secrets stored in the vault and their use are appropriately protected._  

<br/>

### cxapp Vault
The package includes a simple hierarchical vault using file-based storage.

The cxapp property `VAULT` equal to `LOCAL` enables the cxapp vault. 

The vault secret storage directory is specified by the cxapp property `VAULT.DATA`.

A secret name is composed of the characters A-Z, digits 0-9 and punctuation 
underscore `_`and dash `-` with the hierarchy levels delimited by a forward 
slash `/`. The leading forward slash represents the root of the secret hierarchy.

Currently, the package does not contain functions or methods to store secrets. 
A secret is stored as a single line as clear-text in a text file with name
corresponding to the last hierarchical level of the secret, i.e. secret value 
for `/path/to/mysecret` would be stored in the text file 
`<vault.data>/path/to/mysecret`, no file extension (for now).

See help for `cxapp::.cxapp_vaultlocal()` for further details.

<br/>

### Azure Key Vault
The package provides experimental support for Azure Key Vault.
 
The Azure Key Vault is not hierarchical but can use a path notation with 
forward slash as separator to represent a crude hierarchy. The forward 
slashes are translated to underscores mimicking a hierarchical reference 
structure.

The levels of the hierarchy and the secret name consists of the characters
A-Z, 0-9 and punctuation dash (-), underscore '_' and period '.'.
 
The Azure Key Vault relies on multiple cxapp properties. The Azure Key Vault 
service is enabled by setting the cxapp property `VAULT` to the value `AZUREKV`.

The cxapp property `AZUREKV.URL` defines the Azure Key Vault connection URL. 
 
The following properties are used to connect and retrieve a temporary access 
token. All are required.
 
* `AZUREKV.OAUTH.URL` is the URL for Microsoft OAuth service.
* `AZUREKV.OAUTH.CLIENTID` is the OAuth client ID.
* `AZUREKV.OAUTH.CLIENTSECRET` is the client secret.
* `AZUREKV.OAUTH.SCOPE` is the authentication scope. 
 
See help for `cxapp::.cxapp_vaultazurekv()` for further details.

<br/>


