{-# LANGUAGE OverloadedStrings #-}

module Airbrake.DSL where

import Text.Blaze.Internal

notice = Parent "notice" "<notice" "</notice>"

name = Parent "name" "<name" "</name>"

notifier = Parent "notifier" "<notifier" "</notifier>"

api_key = Parent "api-key" "<api-key" "</api-key>"

version = Parent "version" "<version" "</version>"

url = Parent "url" "<url" "</url>"

class_ = Parent "class" "<class" "</class>"

error = Parent "error" "<error" "</error>"

message = Parent "message" "<message" "</message>"

backtrace = Parent "backtrace" "<backtrace" "</backtrace>"

line = Leaf "line" "<line" " />"

file = attribute "file" " file=\""

number = attribute "number" " number=\""

server_environment = Parent "server-environment" "<server-environment" "</server-environment>"

environment_name = Parent "environment-name" "<environment-name" "</environment-name>"

app_version = Parent "app-version" "<app-version" "</app-version>"

project_root = Parent "project-root" "<project-root" "</project-root>"

request = Parent "request" "<request" "</request>"

cgi_data = Parent "cgi-data" "<cgi-data" "</cgi-data>"

action = Parent "action" "<action" "</action>"

component = Parent "component" "<component" "</component>"

var = Parent "var" "<var" "</var>"

key = attribute "key" " key=\""

nversion = attribute "version" " version=\""
