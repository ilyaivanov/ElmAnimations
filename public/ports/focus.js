function registerFocusPorts(ports) {
    ports.onEditStart.subscribe(function (id) {
        const getElementToFocus = () => document.getElementById("input" + id);

        const elem = getElementToFocus();
        if (elem) {
            elem.focus()
        } else {
            const elementToObserve = document.body;
            var observer = new MutationObserver(function () {
                const elem = getElementToFocus();
                if (elem) {
                    elem.focus();
                    observer.disconnect();
                }
            });

            observer.observe(elementToObserve, {subtree: true, childList: true});
        }

    });

    window.addEventListener('keyup', function (e) {
        // I'm filtering out all Esc and Enter key events while not editing anything
        // because I don't want to introduce a huge amount of None events in the Elm action store
        var isEditingNode = document.getElementsByClassName("row-title-input").length > 0;
        var isWaitingForKey = ['Escape', 'Enter'].indexOf(e.key) >= 0;
        if (isEditingNode && isWaitingForKey) {
            ports.onWindowKeyUp.send(e);
        }
    });
}
