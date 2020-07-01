function registerFirebasePorts(ports) {
  ports.generateNewId.subscribe(() => {
    console.log('generateNewId')
    ports.gotNewId.send(Math.random() + "");
  });
}
