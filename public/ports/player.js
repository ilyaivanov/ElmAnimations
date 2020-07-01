var tag = document.createElement('script');

tag.src = "https://www.youtube.com/iframe_api";
var firstScriptTag = document.getElementsByTagName('script')[0];
firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);


function registerPlayerPorts(ports) {
  var player;
  var interval;
  ports.play.subscribe(function (videoId) {
    if (!player)
      player = init(videoId);
    else
      player.loadVideoById(videoId);


    function init(initialVideo) {
      return new YT.Player('youtubePlayer', {
        height: '100%',
        width: '100%',
        videoId: initialVideo,
        playerVars: {'autoplay': 1 /*, 'controls': 0 */},
        events: {
          'onReady': onPlayerReady,
          'onStateChange': onPlayerStateChange,
        },
      });
    }

    function onPlayerStateChange(event) {
      if (event.data === YT.PlayerState.ENDED) {
        console.log('video ended')
        // ports.onVideoEnded.send(null);
      }
    }

    function onPlayerReady() {
      console.log('registering interval');
      interval = setInterval(tick, 800);
    }

    function tick() {
      if (player.getPlayerState() === YT.PlayerState.PLAYING) {
        // ports.onVideoProgress.send({
        //   currentTime: player.getCurrentTime(),
        //   duration: player.getDuration(),
        // });
      }
    }
  });
}
