var fragment = document.createDocumentFragment(),
    div      = document.createElement('div');

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
}

navigator.mediaDevices.getUserMedia(constraints).
    then(onSuccess).
    catch(onError);
