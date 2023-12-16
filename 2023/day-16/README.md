## day 16

Instead of installing the toolchain, I decided to use a swift container: https://www.swift.org/install/linux/#installation-via-docker

```
docker run --privileged --interactive --tty \
    -v ./:/home/ponzif --rm \
    swift:latest /bin/bash
```

swift package init --name day-16 --type library

run test with:
```
swift test
```


sudo apt install clang libpython2.7 libpython2.7-dev
wget https://swift.org/builds/swift-5.8-release/ubuntu2004/swift-5.8-RELEASE/swift-5.8-RELEASE-ubuntu20.04.tar.gz
tar xzf swift-5.8-RELEASE-ubuntu20.04.tar.gz
sudo mv swift-5.8-RELEASE-ubuntu20.04 /usr/share/swift
echo "export PATH=/usr/share/swift/usr/bin:$PATH" >> ~/.bashrc
source ~/.bashrc


