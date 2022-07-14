#!/bin/bash

sudo chown manuelpadilla -R .
sudo find . -type d -exec chmod 755 {} \;
sudo find . -type f -exec chmod u+w  {} \;
sudo find . -type f -exec chmod +x {} \;