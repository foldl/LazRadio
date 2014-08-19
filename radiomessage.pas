unit RadioMessage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // ParamH: TRadioDataStream
  // ParamL: (Port shl 16) or (index of data)
  // Note: call TRadioDataStream.Release after processed
  RM_DATA            = 0;
  // RM_DATA_DONE       = 1;  // this is stupid

  // ParamH: Frequency   (0)    ParamL: in Hz
  // ParamH: Sample rate (1)    ParamL: in samples per second
  // ParamH: Bandwidth   (2)    ParamL: in Hz
  // ParamH: Phase ad    (3)    ParamL: in rad (cast from Float)
  RM_SET_FEATURE     = 2;
                     RM_FEATURE_FREQ        = 0;
                     RM_FEATURE_SAMPLE_RATE = 1;
                     RM_FEATURE_BANDWIDTH   = 2;
                     RM_FEATURE_PHASE_ADJ   = 3;

  // ParamH: RUN   (0)    ParamL: ignore
  // ParamH: PAUSE (1)    ParamL: ignore
  // ParamH: RESET (2)    ParamL: ignore
  RM_CONTROL         = 3;
                     RM_CONTROL_RUN   = 0;
                     RM_CONTROL_PAUSE = 1;
                     RM_CONTROL_RESET = 2;

  // ParamH: timer id    ParamL: timer interval
  RM_TIMER           = 4;

  // ParamH: error   (0)    ParamL: code
  // ParamH: warning (1)    ParamL: code
  // ParamH: info    (2)    ParamL: code
  // ParamH: debug   (3)    ParamL: code
  RM_REPORT          = 5;

  // ParamH: module id (high) timer id (low)  ParamL: timer interval
  RM_CREATE_TIMER    = 6;

  // ParamH: module id (high) timer id (low)  ParamL: ignore
  RM_DELETE_TIMER    = 7;

  RM_CONFIGURE       = 8;

  RM_SHOW_MAIN_GUI   = 9;

  RM_AUDIO_IN_START = 100;   // ParamH = dev id; ParamL = samples per sec
  RM_AUDIO_IN_STOP  = 101;

  RM_AUDIO_OUT_FMT  = 110;  // ParamH as follows:
                    AUDIO_OUT_FMT_MONO_I = 0;
                    AUDIO_OUT_FMT_MONO_Q = 1;
                    AUDIO_OUT_FMT_STEREO_IQ = 2;   // I -> left, Q -> right
                    AUDIO_OUT_FMT_STEREO_QI = 3;   // Q -> left, I -> right
  RM_AUDIO_OUT_GAIN      = 111; // ParamH: gain in dB (cast from Integer)
  RM_AUDIO_OUT_GAIN_AUTO = 112; // auto adjust gain for one-shot

  RM_DUMP_START = 120;              // ParamH: TStream, ParamL: MaxSize
  RM_DUMP_STOP  = 121;              // no param, TStream is closed here

  RM_DUMP_PLAYER_START = 130;       // ParamH: TStream
  RM_DUMP_PLAYER_STOP  = 131;       // no param, TStream is closed here

  RM_FILTER_SET      = 140;             // ParamH: Coeff(PComplex); ParamL: Filter taps
  RM_FILTER_REDESIGN = 141;         // Apply RM_FILTER_CONFIG settings
  RM_FILTER_CONFIG   = 142;
                   FILTER_TYPE       = 0;   // ParamL: TFilterType
                   FILTER_OMEGA      = 1;   // ParamL: Omega (Hz) (cast from Integer)
                   FILTER_BANDWIDTH  = 2;   // ParamL: Bandwidth (Hz)
                   FILTER_TAPS       = 3;   // ParamL: Taps
                   FILTER_WINDOW     = 4;   // ParamL: TWindowFunction
                   FILTER_WINDOW_PARAM = 5; // ParamL: param1 of window function (Single)
                   FILTER_COEFF_DOMAIN = 6;   // ParamL: filter coefficients domain (complex (0 - default) or real (1))

                                       FILTER_COEFF_DOMAIN_COMPLEX = 0;
                                       FILTER_COEFF_DOMAIN_REAL    = 1;
  RM_FILTER_USE_BAND_SELECT = 143;  // ParamH: Band index, 0, 1,... (see RM_SPECTRUM_BAND_SELECT_x); ParamL: reserved

  RM_SPECTRUM_CFG = 150;
                  SET_WND_FUNC      = 0;
                  SET_OVERLAP_PER   = 1;
                  SET_FFT_SIZE      = 2;
                  GUI_RESET         = 3;  // gui resize
                  SET_Y_RANGE       = 4;  // y range in dB
                  SET_AUTO_Y        = 5;  // set y range automatically, ParamL: enable (1), disable (0)
                  SET_SPAN          = 6;  // set x span in Hz (full span: sample rate / 2 (-1))
                  SET_CENTER_FREQ   = 7;  // set center frequency in x span (freq of the input data stream)
                  SET_Y_MAX         = 8;
                  SET_WATERFALL_TICK = 9; // draw waterfall tick per ParamL seconds (zero = OFF)
                  SET_DRAW_MIN_INTERVAL = 10; // minimal interval between updates in milli-second
                  SET_DATA_DOMAIN   = 11; // set data domain: complex(0: default), real (1)

                                    SPECTRUM_DATA_DOMAIN_COMPLEX = 0;
                                    SPECTRUM_DATA_DOMAIN_REAL    = 1;

  // ParamH: Baseband low freq (Hz), ParamL: Baseband high freq (Hz)
  RM_SPECTRUM_BAND_SELECT_1 = 151;
  RM_SPECTRUM_BAND_SELECT_2 = 152;
  RM_SPECTRUM_BAND_SELECT_3 = 153;
  RM_SPECTRUM_BAND_SELECT_4 = 154;

  // ParamH: Device index
  RM_RTL_START = 160;
  RM_RTL_STOP  = 161;

  RM_RTL_DEV_CTL = 162;
                 RTL_SET_FREQ_CORRECTION  = 0;   // ParamL = (ppm)
                 RTL_SET_TUNNER_GAIN_MODE = 1;   // ParamL: 0 (auto), 1 (manual), 2 (software)
                                          RTL_TUNNER_GAIN_AUTO     = 0;
                                          RTL_TUNNER_GAIN_MANUAL   = 1;
                                          RTL_TUNNER_GAIN_SOFTWARE = 2;
                 RTL_SET_TUNNER_GAIN      = 2;   // ParamL = gain (Integer)
                 RTL_SET_TUNNER_IF_GAIN   = 3;   // ParamL = Stage << 16 | Gain (tenth dB)
                 RTL_SET_AGC_MODE         = 4;   // ParamL: Enable (1) or disable (0) the internal digital AGC
                 RTL_SET_DIRECT_SAMPLING  = 5;   // ParamL: 0 means disabled, 1 I-ADC input enabled, 2 Q-ADC input enabled
                                          RTL_SAMPLING_QUAD = 0;
                                          RTL_SAMPLING_I    = 1;
                                          RTL_SAMPLING_Q    = 2;
                 RTL_SET_OFFSET_TUNNING   = 6;   // ParamL: 0 means disabled, 1 enabled

  RM_OSC_WAVE = 170;
                  SET_WAVE_SIN      = 0;
                  SET_WAVE_RECT     = 1;  // ParamL = DutyRadio (in percentage)
                  SET_WAVE_TRIANGLE = 2;  // ParamL = VertexPostition
                                          //          (in percentage: 0 = reverse sawtooth; 50 = triangle; 100 = (almost) sawtooth)

  // reconfig, ParamH: output rate (Hz); ParamL: LPF cutoff frequency (Hz)
  RM_RESAMPLING_CFG = 180;
  RM_RESAMPLING_USE_BAND_SELECT = 181; // ParamH: Band index, 0, 1,... (see RM_SPECTRUM_BAND_SELECT_x); ParamL: reserved

  RM_FREQMIXER_USE_BAND_SELECT = 190;  // ParamH: Band index, 0, 1,... (see RM_SPECTRUM_BAND_SELECT_x); ParamL: reserved

  RM_AUDIOMIXER_SET_STREAM_OUPUT = 200;      // ParamH: Stream index; ParamL: output type
                  AUDIOMIXER_STREAM_OUTPUT_OFF   = 0;
                  AUDIOMIXER_STREAM_OUTPUT_IQ_IQ = 1;
                  AUDIOMIXER_STREAM_OUTPUT_QI_QI = 2;
                  AUDIOMIXER_STREAM_OUTPUT_IQ_I  = 3;
                  AUDIOMIXER_STREAM_OUTPUT_IQ_Q  = 4;
                  AUDIOMIXER_STREAM_OUTPUT_I_I   = 5;
                  AUDIOMIXER_STREAM_OUTPUT_I_Q   = 6;
                  AUDIOMIXER_STREAM_OUTPUT_Q_I   = 7;
                  AUDIOMIXER_STREAM_OUTPUT_Q_Q   = 8;

  RM_AUDIOMIXER_SET_STREAM_TOTAL_GAIN       = 201;      // ParamH: Stream index; ParamL: total gain (integer in tenth dB)
  RM_AUDIOMIXER_SET_STREAM_BASS_GAIN        = 202;      // ParamH: Stream index; ParamL: bass gain (integer in tenth dB)
  RM_AUDIOMIXER_SET_STREAM_TREBLE_GAIN      = 203;      // ParamH: Stream index; ParamL: tremble gain (integer in tenth dB)
  RM_AUDIOMIXER_CFG                         = 204;      // configure
                  AUDIOMIXER_STREAM_NUM  = 0;           // ParamL: number of input streams

  RM_FMRECEIVER_CFG                         = 210;      // configure
                 FMRECEIVER_MODE           = 0;        // set working mode
                          FMRECEIVER_STEREO = 0;
                          FMRECEIVER_MONO   = 1;
                 FMRECEIVER_DEEMPHASIS_TIME_CONSTANT = 1; // set de-emphasis time constant, ParamL: time constant in us, typical value 50/75us

  RM_USER            = 1000;

type

  { TRadioMessage }

  TRadioMessage = record
    Sender: string; // TObject;
    Id: Integer;
    ParamH: PtrUInt;
    ParamL: PtrUInt;
  end;


implementation

end.

