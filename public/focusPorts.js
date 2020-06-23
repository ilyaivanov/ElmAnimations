function registerFocusPorts(ports){
  ports.onEditStart.subscribe(function (id) {
    var elementToObserve = document.body;

    var observer = new MutationObserver(function () {
      const elem = document.getElementById("input" + id);
      if (elem) {
        elem.focus();
        observer.disconnect();
      }
    });

    observer.observe(elementToObserve, {subtree: true, childList: true});
  });

  window.addEventListener('keyup', function (e) {
    var isEditingNode = document.getElementsByClassName("row-title-input").length > 0;
    var isWaitingForKey = ['Escape', 'Enter'].indexOf(e.key) >= 0;
    if (isEditingNode && isWaitingForKey) {
      ports.onWindowKeyUp.send(e);
    }
  });
}
