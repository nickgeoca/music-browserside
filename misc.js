function jsQuadraticCurveTo(ctx, cpx, cpy, x, y) {
    ctx.quadraticCurveTo(cpx, cpy, x, y);
}

// https://github.com/mudcube/MIDI.js/blob/master/js/midi/loader.js
function jsMidiLoadPlugin(midi){
    midi.loadPlugin({
	soundfontUrl: "./soundfont/",
	instrument  : "acoustic_grand_piano",
	onsuccess   : function() { }
	});
}

// https://github.com/mudcube/MIDI.js/blob/master/js/midi/plugin.webmidi.js
function jsMidiNoteOn(midi, channel, note, velocity, delay) {
    midi.noteOn(channel, note, velocity, delay);
}    
function jsMidiNoteOff(midi, channel, note, delay) {
    midi.noteOff(channel, note, delay);
}    
