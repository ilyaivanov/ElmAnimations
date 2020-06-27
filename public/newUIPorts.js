function registerNewUIPorts(ports) {

  ports.scrollToTop.subscribe(() => {
    console.log('scrollToTop');
    scroll();
  });

  function scroll() {
    window.scroll({
      behavior: 'smooth',
      top: 0,
    });
  }
}
