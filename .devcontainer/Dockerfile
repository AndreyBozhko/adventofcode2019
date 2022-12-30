FROM clojure:latest

RUN groupadd docker && \
    useradd -r -u 1001 -g docker vscode && \
    mkdir -p /home/vscode && \
    chown -R vscode:docker /home/vscode
USER vscode
