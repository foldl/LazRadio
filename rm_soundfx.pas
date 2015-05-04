unit rm_soundfx;

{$mode objfpc}{$H+}

{
echo - to simulate the effect of reverberation in a large hall or cavern,
one or several delayed signals are added to the original signal.
To be perceived as echo, the delay has to be of order 35 milliseconds or above.
Short of actually playing a sound in the desired environment, the effect of echo
can be implemented using either digital or analog methods. Analog echo effects are
implemented using tape delays and/or spring reverbs. When large numbers of delayed
signals are mixed over several seconds, the resulting sound has the effect of
being presented in a large room, and it is more commonly called reverberation or reverb for short.

flanger - to create an unusual sound, a delayed signal is added to the original
signal with a continuously variable delay (usually smaller than 10 ms).
This effect is now done electronically using DSP, but originally the effect was
created by playing the same recording on two synchronized tape players, and then
mixing the signals together. As long as the machines were synchronized, the mix
would sound more-or-less normal, but if the operator placed his finger on the flange
of one of the players (hence "flanger"), that machine would slow down and its signal
would fall out-of-phase with its partner, producing a phasing effect.
Once the operator took his finger off, the player would speed up until its tachometer
was back in phase with the master, and as this happened, the phasing effect would appear t
o slide up the frequency spectrum. This phasing up-and-down the register can be
performed rhythmically.

phaser - another way of creating an unusual sound; the signal is split, a portion
is filtered with an all-pass filter to produce a phase-shift, and then the unfiltered
and filtered signals are mixed. The phaser effect was originally a simpler implementation
of the flanger effect since delays were difficult to implement with analog equipment.
Phasers are often used to give a "synthesized" or electronic effect to natural sounds,
such as human speech. The voice of C-3PO from Star Wars was created by taking
the actor's voice and treating it with a phaser.

chorus - a delayed signal is added to the original signal with a constant delay.
The delay has to be short in order not to be perceived as echo, but above 5 ms to be
audible. If the delay is too short, it will destructively interfere with the un-delayed
signal and create a flanging effect. Often, the delayed signals will be slightly
pitch shifted to more realistically convey the effect of multiple voices.

overdrive- effects such as the use of a fuzz box can be used to produce distorted
sounds, such as for imitating robotic voices or to simulate distorted radiotelephone
traffic (e.g., the radio chatter between starfighter pilots in the science fiction
film Star Wars). The most basic overdrive effect involves clipping the signal when
its absolute value exceeds a certain threshold.

pitch shift - similar to pitch correction, this effect shifts a signal up or down
in pitch. For example, a signal may be shifted an octave up or down. This is usually
applied to the entire signal, and not to each note separately. One application of
pitch shifting is pitch correction. Here a musical signal is tuned to the correct
pitch using digital signal processing techniques. This effect is ubiquitous in karaoke
machines and is often used to assist pop singers who sing out of tune. It is also
used intentionally for aesthetic effect in such pop songs as Cher's Believe and
Madonna's Die Another Day.

time stretching - the opposite of pitch shift, that is, the process of changing
the speed of an audio signal without affecting its pitch.

resonators - emphasize harmonic frequency content on specified frequencies. 

robotic voice effects are used to make an actor's voice sound like a synthesized
human voice.

modulation - to change the frequency or amplitude of a carrier signal in relation
to a predefined signal.Ring modulation, also known as amplitude modulation, is an
effect made famous by Doctor Who's Daleks and commonly used throughout sci-fi.

compression - the reduction of the dynamic range of a sound to avoid unintentional
fluctuation in the dynamics. Level compression is not to be confused with audio
data compression, where the amount of data is reduced without affecting the amplitude
of the sound it represents.

reverse echo - a swelling effect created by reversing an audio signal and recording
echo and/or delay whilst the signal runs in reverse. When played back forward the
last echos are heard before the effected sound creating a rush like swell preceding
and during playback.[2]
}
interface

uses
  Classes, SysUtils;

implementation

end.

