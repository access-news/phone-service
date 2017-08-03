var fragment = document.createDocumentFragment(),
    div      = document.createElement('div');

var audioContext = new AudioContext();

div.innerHTML = 
  '<button class="record">Record</button>   \
   <button class="pause">Pause</button>     \
   <button class="resume">Resume</button>   \
   <button class="stop">Stop</button>       \
   <ul></ul>';

fragment.appendChild(div);

document.body.insertBefore(fragment, document.body.childNodes[0]);

var record = document.querySelector('.record'),
    stop   = document.querySelector('.stop'),
    pause  = document.querySelector('.pause'),
    resume = document.querySelector('.resume');

stop.disabled = pause.disabled = resume.disabled = true;

var constraints = {audio: true};
var chunks = [];

var onError = function(err) {
    console.log('The following error occured: ' + err);
}

var onSuccess = function(stream) {
    var mediaRecorder = new MediaRecorder(stream);

    mediaRecorder.ondataavailable = function(event) {
        console.log(' Recorded chunk of size ' + event.data.size + "B");
        chunks.push(event.data);
    }

    record.onclick = function(_event) {
        mediaRecorder.start(100)

        record.disabled = resume.disabled = true;
        stop.disabled = pause.disabled = false;
    }

    pause.onclick = function(_event) {
        mediaRecorder.pause();

        stop.disabled = resume.disabled = false;
        record.disabled = pause.disabled = true;
    }

    resume.onclick = function(_event) {
        mediaRecorder.resume();

        record.disabled = resume.disabled = true;
        stop.disabled = pause.disabled = false;
    }

    // mediaRecorder.onresume = function(_event) {
    //     mediaRecorder.requestData();
    // }

    stop.onclick = function(_event) {
        mediaRecorder.stop();

        stop.disabled = pause.disabled = resume.disabled = true;
        record.disabled = false;
    }

    mediaRecorder.onstop = function(_event) {
        // from https://github.com/Mido22/MediaRecorder-sample/blob/master/script.js
        function makeLink(){
            let blob = new Blob(chunks, {type: 'audio/webm' })
              , url = URL.createObjectURL(blob)
              , audio = document.createElement('audio')
              , hf = document.createElement('a')
              , li = document.createElement('li')
            ;
            audio.controls = true;
            audio.preload = 'auto';
            audio.src = url;
            hf.href = url;
            hf.download = `${Date.now()}.ogg`;
            hf.innerHTML = `Download ${hf.download}`;
            li.appendChild(audio);
            li.appendChild(hf);
            div.querySelector('ul').appendChild(li);
        }
        makeLink();
        chunks = [];
    }

    // EXPERIMENT 1
    // adding lower level stuff besides recording
    // GAP: is this the right way when doing recording as well?

        // var streamSource = audioContext.createMediaStreamSource(stream);
        // var csp = audioContext.createScriptProcessor(0,2,2);

        // streamSource.connect(csp);
        // csp.connect(audioContext.destination);
        // csp.onaudioprocess = function(event) {
        //     var audioData = event.inputBuffer.getChannelData(0);
        //     console.log(audioData);
        // }

    // EVAL: One unexpected result was that once the media source (i.e. the mic in
    //       in this context) was allowed to be accessed, the data on the console
    //       started flowing. Of course, this is a stream object, so what else did
    //       I expect?
}

// EXPERIMENT 2
// using `createMediaElementSource` this time
    // document.querySelector('#experiment').onclick = function(_event) {
    //     var audioElem = document.querySelector('audio');
    //     var elemSource = audioContext.createMediaElementSource(audioElem);
    //     var csp = audioContext.createScriptProcessor(0,2,2);

    //     elemSource.connect(csp);
    //     csp.connect(audioContext.destination);
    //     csp.onaudioprocess = function(event) {
    //         var audioData = event.inputBuffer.getChannelData(0);
    //         console.log(audioData);
    //     }
    // }
// EVAL: The result was unexpected but I learned something again:
//       `console.log()` kept spitting out Float32Array arrays that were
//       zeroed out but when hit play on the `<audio>` element, data
//       started to flow until it was stopped.

// EXPERIMENT 3
// For some reason it did not sink in that create*Source returns an
// AudioBufferSourceNode object that has `start()` and `stop()` methods.
// Here we go:
    var absnStartButton = document.createElement('button'),
        absnStopButton  = document.createElement('button');

    
    document.querySelector('#experiment').onclick = function(_event) {
        var audioElem = document.querySelector('audio');
        var elemSource = audioContext.createMediaElementSource(audioElem);
        var csp = audioContext.createScriptProcessor(0,2,2);

        elemSource.connect(csp);
        csp.connect(audioContext.destination);
        csp.onaudioprocess = function(event) {
            var audioData = event.inputBuffer.getChannelData(0);
            console.log(audioData);
        }
    }


navigator.mediaDevices.getUserMedia(constraints).
    then(onSuccess).
    catch(onError);
