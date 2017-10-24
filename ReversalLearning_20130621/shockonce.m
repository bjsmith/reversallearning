function shockonce
unix('../k8055 -P:3 -A1:0 -A2:0')
unix('../k8055 -P:3 -A1:255 -A2:255')
unix('../k8055 -P:3 -A1:0 -A2:0')