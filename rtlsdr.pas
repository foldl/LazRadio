{
 * rtl-sdr, turns your Realtek RTL2832 based DVB dongle into a SDR receiver
 * Copyright (C) 2012-2013 by Steve Markgraf <steve@steve-m.de>
 * Copyright (C) 2012 by Dimitri Stolnikov <horiz0n@gmx.net>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Ported to Pascal by Li Zhengji (using Pascal naming convention)
}
unit RtlSdr;

{$mode objfpc}{$H+}
{$calling cdecl}

interface

uses
  Classes, SysUtils;

const
  RTLLIB = 'rtlsdr.dll';

type

  TRtlSdrDev = record
  end;
  PRtlSdrDev = ^TRtlSdrDev;

  TRtlSdrTuner = (
	RTLSDR_TUNER_UNKNOWN,
	RTLSDR_TUNER_E4000,
	RTLSDR_TUNER_FC0012,
	RTLSDR_TUNER_FC0013,
	RTLSDR_TUNER_FC2580,
	RTLSDR_TUNER_R820T,
	RTLSDR_TUNER_R828D
  );

  TRtlSdrReadAsyncCB = procedure(Buf: PByte; const Len: Cardinal; Ctx: Pointer);

function RtlSdrGetDeviceCount(): Cardinal; external RTLLIB name 'rtlsdr_get_device_count';

function RtlSdrGetDeviceName(Index: Cardinal): PChar; external RTLLIB name 'rtlsdr_get_device_name';

{!
 * Get USB device strings.
 *
 * NOTE: The string arguments must provide space for up to 256 bytes.
 *
 * \param index the device index
 * \param manufact manufacturer name, may be NULL
 * \param product product name, may be NULL
 * \param serial serial number, may be NULL
 * \return 0 on success
}
function RtlSdrGetDeviceUSBString(const Index: Cardinal; Manufacture, Product, Serial: PChar): Integer; external RTLLIB name 'rtlsdr_get_device_usb_strings';

{!
 * Get device index by USB serial string descriptor.
 *
 * \param serial serial string of the device
 * \return device index of first device where the name matched
 * \return -1 if name is NULL
 * \return -2 if no devices were found at all
 * \return -3 if devices were found, but none with matching name
}
function RtlSdrGetIndexBySerial(const Serial: PChar): Integer; external RTLLIB name 'rtlsdr_get_index_by_serial';

function RtlSdrOpen(var Dev: PRtlSdrDev; const Index: Cardinal): Integer; external RTLLIB name 'rtlsdr_open';

function RtlSdrClose(Dev: PRtlSdrDev): Integer; external RTLLIB name 'rtlsdr_close';

{ configuration functions }

{!
 * Set crystal oscillator frequencies used for the RTL2832 and the tuner IC.
 *
 * Usually both ICs use the same clock. Changing the clock may make sense if
 * you are applying an external clock to the tuner or to compensate the
 * frequency (and samplerate) error caused by the original (cheap) crystal.
 *
 * NOTE: Call this function only if you fully understand the implications.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param rtl_freq frequency value used to clock the RTL2832 in Hz
 * \param tuner_freq frequency value used to clock the tuner IC in Hz
 * \return 0 on success
}
function RtlSdrSetXtalFreq(Dev: PRtlSdrDev; RtlFreq, TunerFreq: Cardinal): Integer; external RTLLIB name 'rtlsdr_set_xtal_freq';

{!
 * Get crystal oscillator frequencies used for the RTL2832 and the tuner IC.
 *
 * Usually both ICs use the same clock.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param rtl_freq frequency value used to clock the RTL2832 in Hz
 * \param tuner_freq frequency value used to clock the tuner IC in Hz
 * \return 0 on success
}
function RtlSdrGetXtalFreq(Dev: PRtlSdrDev; var RtlFreq, TunerFreq: Cardinal): Integer; external RTLLIB name 'rtlsdr_get_xtal_freq';

{!
 * Get USB device strings.
 *
 * NOTE: The string arguments must provide space for up to 256 bytes.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param manufact manufacturer name, may be NULL
 * \param product product name, may be NULL
 * \param serial serial number, may be NULL
 * \return 0 on success
}
function RtlSdrGetUSBStrings(Dev: PRtlSdrDev; Manufacture, Product, Serial: PChar): Integer; external RTLLIB name 'rtlsdr_get_usb_strings';

{!
 * Write the device EEPROM
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param data buffer of data to be written
 * \param offset address where the data should be written
 * \param len length of the data
 * \return 0 on success
 * \return -1 if device handle is invalid
 * \return -2 if EEPROM size is exceeded
 * \return -3 if no EEPROM was found
}
function RtlSdrWriteEEPROM(Dev: PRtlSdrDev; Data: PByte; Offset: Byte; Len: Word): Integer; external RTLLIB name 'rtlsdr_write_eeprom';

{!
 * Read the device EEPROM
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param data buffer where the data should be written
 * \param offset address where the data should be read from
 * \param len length of the data
 * \return 0 on success
 * \return -1 if device handle is invalid
 * \return -2 if EEPROM size is exceeded
 * \return -3 if no EEPROM was found
}
function RtlSdrReadEEPROM(Dev: PRtlSdrDev; Data: PByte; Offset: Byte; Len: Word): Integer; external RTLLIB name 'rtlsdr_read_eeprom';

function RtlSdrSetCenterFreq(Dev: PRtlSdrDev; const Freq: Cardinal): Integer; external RTLLIB name 'rtlsdr_set_center_freq';

{!
 * Get actual frequency the device is tuned to.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \return 0 on error, frequency in Hz otherwise
}

function RtlSdrGetCenterFreq(Dev: PRtlSdrDev): Cardinal; external RTLLIB name 'rtlsdr_get_center_freq';

{!
 * Set the frequency correction value for the device.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param ppm correction value in parts per million (ppm)
 * \return 0 on success
}
function RtlSdrSetFreqCorrection(Dev: PRtlSdrDev; const PPM: Integer): Integer; external RTLLIB name 'rtlsdr_set_freq_correction';

{!
 * Get actual frequency correction value of the device.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \return correction value in parts per million (ppm)
}
function RtlSdrGetFreqCorrection(Dev: PRtlSdrDev): Integer; external RTLLIB name 'rtlsdr_get_freq_correction';

{!
 * Get the tuner type.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \return RTLSDR_TUNER_UNKNOWN on error, tuner type otherwise
}
function RtlSdrGetTunerType(const Index: Integer): TRtlSdrTuner; overload;
function RtlSdrGetTunerType(Dev: PRtlSdrDev): TRtlSdrTuner; external RTLLIB name 'rtlsdr_get_tuner_type';
function FormatTunerType(T: TRtlSdrTuner): string;


{!
 * Get a list of gains supported by the tuner.
 *
 * NOTE: The gains argument must be preallocated by the caller. If NULL is
 * being given instead, the number of available gain values will be returned.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param gains array of gain values. In tenths of a dB, 115 means 11.5 dB.
 * \return <= 0 on error, number of available (returned) gain values otherwise
}
function RtlSdrGetTunerGains(Dev: PRtlSdrDev; Gains: PInteger): Integer; external RTLLIB name 'rtlsdr_get_tuner_gains';

{!
 * Set the gain for the device.
 * Manual gain mode must be enabled for this to work.
 *
 * Valid gain values (in tenths of a dB) for the E4000 tuner:
 * -10, 15, 40, 65, 90, 115, 140, 165, 190,
 * 215, 240, 290, 340, 420, 430, 450, 470, 490
 *
 * Valid gain values may be queried with \ref rtlsdr_get_tuner_gains function.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param gain in tenths of a dB, 115 means 11.5 dB.
 * \return 0 on success
}
function RtlSdrSetTunerGain(Dev: PRtlSdrDev; Gain: Integer): Integer; external RTLLIB name 'rtlsdr_set_tuner_gain';

{!
 * Get actual gain the device is configured to.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \return 0 on error, gain in tenths of a dB, 115 means 11.5 dB.
}
function RtlSdrGetTunerGain(Dev: PRtlSdrDev): Integer; external RTLLIB name 'rtlsdr_get_tuner_gain';

{!
 * Set the intermediate frequency gain for the device.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param stage intermediate frequency gain stage number (1 to 6 for E4000)
 * \param gain in tenths of a dB, -30 means -3.0 dB.
 * \return 0 on success
}
function RtlSdrSetTunerIfGain(Dev: PRtlSdrDev; const Stage, Gain: Integer): Integer; external RTLLIB name 'rtlsdr_set_tuner_if_gain';

{!
 * Set the gain mode (automatic/manual) for the device.
 * Manual gain mode must be enabled for the gain setter function to work.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param manual gain mode, 1 means manual gain mode shall be enabled.
 * \return 0 on success
}
function RtlSdrSetTunerGainMode(Dev: PRtlSdrDev; const Manual: Integer): Integer; external RTLLIB name 'rtlsdr_set_tuner_gain_mode';

{
(samp_rate <= 225000)   ((samp_rate > 300000) && (samp_rate <= 900000))   (samp_rate > 3200000)
}
{ this will select the baseband filters according to the requested sample rate}
function RtlSdrSetSampleRate(Dev: PRtlSdrDev; const Rate: Cardinal): Integer; external RTLLIB name 'rtlsdr_set_sample_rate';

{!
 * Get actual sample rate the device is configured to.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \return 0 on error, sample rate in Hz otherwise
}
function RtlSdrGetSampleRate(Dev: PRtlSdrDev): Cardinal; external RTLLIB name 'rtlsdr_get_sample_rate';

{!
 * Enable test mode that returns an 8 bit counter instead of the samples.
 * The counter is generated inside the RTL2832.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param test mode, 1 means enabled, 0 disabled
 * \return 0 on success
}
function RtlSdrSetTestMode(Dev: PRtlSdrDev; const On1: Integer): Integer; external RTLLIB name 'rtlsdr_set_testmode';

{!
 * Enable or disable the internal digital AGC of the RTL2832.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param digital AGC mode, 1 means enabled, 0 disabled
 * \return 0 on success
}
function RtlSdrSetAgcMode(Dev: PRtlSdrDev; const On1: Integer): Integer; external RTLLIB name 'rtlsdr_set_agc_mode';

{!
 * Enable or disable the direct sampling mode. When enabled, the IF mode
 * of the RTL2832 is activated, and rtlsdr_set_center_freq() will control
 * the IF-frequency of the DDC, which can be used to tune from 0 to 28.8 MHz
 * (xtal frequency of the RTL2832).
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param on 0 means disabled, 1 I-ADC input enabled, 2 Q-ADC input enabled
 * \return 0 on success
}
function RtlSdrSetDirectSampling(Dev: PRtlSdrDev; const On1: Integer): Integer; external RTLLIB name 'rtlsdr_set_direct_sampling';

{!
 * Get state of the direct sampling mode
 *
 * \param dev the device handle given by rtlsdr_open()
 * \return -1 on error, 0 means disabled, 1 I-ADC input enabled
 *	    2 Q-ADC input enabled
}
function RtlSdrGetDirectSampling(Dev: PRtlSdrDev): Integer; external RTLLIB name 'rtlsdr_get_direct_sampling';

{!
 * Enable or disable offset tuning for zero-IF tuners, which allows to avoid
 * problems caused by the DC offset of the ADCs and 1/f noise.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param on 0 means disabled, 1 enabled
 * \return 0 on success
}
function RtlSdrSetOffsetTuning(Dev: PRtlSdrDev; const On1: Integer): Integer; external RTLLIB name 'rtlsdr_set_offset_tuning';

{!
 * Get state of the offset tuning mode
 *
 * \param dev the device handle given by rtlsdr_open()
 * \return -1 on error, 0 means disabled, 1 enabled
}
function RtlSdrGetOffsetTuning(Dev: PRtlSdrDev): Integer; external RTLLIB name 'rtlsdr_get_offset_tuning';

{ streaming functions}
function RtlSdrResetBuffer(Dev: PRtlSdrDev): Integer; external RTLLIB name 'rtlsdr_reset_buffer';

function RtlSdrReadSync(Dev: PRtlSdrDev; var Buf; const Len: Integer; var NRead: Integer): Integer; external RTLLIB name 'rtlsdr_read_sync';

{!
 * Read samples from the device asynchronously. This function will block until
 * it is being canceled using rtlsdr_cancel_async()
 *
 * \param dev the device handle given by rtlsdr_open()
 * \param cb callback function to return received samples
 * \param ctx user specific context to pass via the callback function
 * \param buf_num optional buffer count, buf_num * buf_len = overall buffer size
 *		  set to 0 for default buffer count (32)
 * \param buf_len optional buffer length, must be multiple of 512,
 *		  set to 0 for default buffer length (16 * 32 * 512)
 * \return 0 on success
}
function RtlSdrReadASync(Dev: PRtlSdrDev; CB: TRtlSdrReadAsyncCB; Ctx: Pointer; BufNum: Cardinal; BufLen: Cardinal): Integer; external RTLLIB name 'rtlsdr_read_async';

{!
 * Cancel all pending asynchronous operations on the device.
 *
 * \param dev the device handle given by rtlsdr_open()
 * \return 0 on success
}
function RtlSdrCancelAsync(Dev: PRtlSdrDev): Integer; external RTLLIB name 'rtlsdr_cancel_async';

implementation

function RtlSdrGetTunerType(const Index: Integer): TRtlSdrTuner;
var
  P: PRtlSdrDev = nil;
begin
  Result := RTLSDR_TUNER_UNKNOWN;
  if RtlSdrOpen(P, Index) <> 0 then Exit;
  Result := RtlSdrGetTunerType(P);
  RtlSdrClose(P);
end;

function FormatTunerType(T: TRtlSdrTuner): string;
begin
   Result := 'UKNOWN';
   case T of
	RTLSDR_TUNER_E4000   : Result := 'E4000';
	RTLSDR_TUNER_FC0012  : Result := 'FC0012';
	RTLSDR_TUNER_FC0013  : Result := 'FC0013';
	RTLSDR_TUNER_FC2580  : Result := 'FC2580';
	RTLSDR_TUNER_R820T   : Result := 'R820T';
	RTLSDR_TUNER_R828D   : Result := 'R828D';
  end;
end;

end.

