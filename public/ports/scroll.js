function registerDragPorts(ports) {
  var isDragging = false;

  ports.startDrag.subscribe(() => {
    isDragging = true;
  });

  ports.endDrag.subscribe(() => {
    isDragging = false;
  });


  //Chrome starts screen from 100 for some reason
  const upperScreen = 100;

  //lower and upper boundaries height when windows starts to scroll
  //assumes linear accelerations depending on how far cursor is from the boundary
  const scrollHeight = 120;
  var scrollSpeed = 0;
  var timeout;
  window.onmousemove = (e) => {
    if (!isDragging)
      return;

    const y = e.screenY;
    const screenHeight = e.view.innerHeight;
    const lowerBoundary = upperScreen + scrollHeight;
    const upperBoundary = screenHeight - scrollHeight + upperScreen;
    const isScrolling = !!timeout;

    const startScrollingIfNeeded = () => {
      if (!isScrolling) {
        timeout = setInterval(scroll, 80);
      }
    };

    if (y <= lowerBoundary) {
      scrollSpeed = (y - lowerBoundary);
      startScrollingIfNeeded();
    } else if (y > lowerBoundary && y < upperBoundary && isScrolling) {
      clearInterval(timeout);
      timeout = 0;
    } else if (y >= upperBoundary) {
      scrollSpeed = (y - upperBoundary);
      startScrollingIfNeeded();
    }
  };

  function scroll() {
    window.scrollBy({
      behavior: 'smooth',
      top: scrollSpeed,
    });
  }
}
