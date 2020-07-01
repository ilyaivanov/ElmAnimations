function registerSearchPorts(ports) {
    ports.findVideos.subscribe(term => {
        console.log('searching', term)

        fetch('https://europe-west1-lean-watch.cloudfunctions.net/getVideos?q=' + term)
            .then(result => result.json())
            .then(response => {
                const items = response.items
                    .filter(item => item.itemType === 'video')
                    .map(item => ({
                        id: item.id,
                        videoId: item.itemId,
                        title: item.name
                    }));
                ports.gotVideos.send(items);
            })
    })
}