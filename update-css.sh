#!/bin/bash
# Simple script to copy CSS changes to static assets
echo "Updating CSS..."
cp assets/css/app.css priv/static/assets/css/app.css
echo "CSS updated successfully!"
echo "File size: $(ls -lh priv/static/assets/css/app.css | awk '{print $5}')"
