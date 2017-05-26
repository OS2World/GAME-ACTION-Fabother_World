/* Raw - Another World Interpreter

   OS/2-specialized code by Andrey Vasilkin, 2014
 */

#include "sys.h"
#include "util.h"

#define INCL_DOSFILEMGR   /* File Manager values */
#define INCL_DOSERRORS    /* DOS Error values    */
#define INCL_WIN
#define INCL_GPI
#define INCL_DOS
#define INCL_ERRORS
#define INCL_OS2MM        /* required for MCI and MMIO headers   */
#include <os2.h>
#define  _MEERROR_H_
#include <mmioos2.h>
#include <os2me.h>
#include <dive.h>
#include <fourcc.h>

#define		WIN_CLIENT_CLASS	"AnotherWorld"
#define		ID_APP_WINDOW		1
#define		INPUT_EVENTS_QUEUE_LEN	8

MRESULT EXPENTRY ClientWndProc(HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2);
LONG APIENTRY AudioEvent(ULONG ulStatus, PMCI_MIX_BUFFER pBuffer, ULONG ulFlags);

struct OS2Stub : System
{
  enum
  {
    SCREEN_W = 320,
    SCREEN_H = 200,
    SOUND_SAMPLE_RATE = 22050,
    SCREEN_DIVE_W = (SCREEN_W*3),
    SCREEN_DIVE_H = (SCREEN_H*3),

    EV_NULL = 0,
    EV_UP = 1,
    EV_KEYUP_UP = 2,
    EV_DOWN = 3,
    EV_KEYUP_DOWN = 4,
    EV_LEFT = 5,
    EV_KEYUP_LEFT = 6,
    EV_RIGHT = 7,
    EV_KEYUP_RIGHT = 8,
    EV_BUTTON = 9,
    EV_KEYUP_BUTTON = 10,
    EV_SAVE = 11,
    EV_LOAD = 12,
    EV_FASTMODE = 13,
    EV_STATESLOT_INC = 14,
    EV_STATESLOT_DEC = 15,
    EV_CODE = 16,
    EV_PAUSE = 17,
    EV_QUIT = 18,
    EV_SWITCH_FULL_SCREEN = 30,
    EV_CHAR = 100
  };

  typedef struct _SYSTIMER {
    HEV			hEv;
    HTIMER		hTimer;
    ULONG		ulDelay;
    BOOL		fCallback;
    TimerCallback	fnCallback;
    PVOID		pParam;
  } SYSTIMER, *PSYSTIMER;

  ULONG			ulDiveBufferNumber;
  HDIVE			hDive;
  PBYTE			pbImgBuf;
  PBYTE			pbOffscreen;

  HAB			hab;
  HMQ			hmq;
  HWND			hwndFrame;
  HPS			hpsClient;
  ULONG			aulEvents[INPUT_EVENTS_QUEUE_LEN];
  ULONG			ulEvWritePos, ulEvReadPos;
  BOOL			fWinFocus;
  PSZ			pszTitle;
  HMTX			hmtxWin;
  HMUX			hmuxTimers;
  HEV			hevWakeup;
  BOOL			fFSMode;
  BOOL			fPause;

  USHORT		usAudioDeviceId;
  MCI_MIXSETUP_PARMS	sMCIMixSetup;
  MCI_BUFFER_PARMS	sMCIBuffer;
  AudioCallback		fnAudioCallback;
  PVOID			pAudioCallbackParam;

  virtual ~OS2Stub() {}

  virtual void init(const char *title);
  virtual void destroy();
  virtual void setPalette(uint8_t s, uint8_t n, const uint8_t *buf);
  virtual void copyRect(uint16_t x, uint16_t y, uint16_t w, uint16_t h, const uint8_t *buf, uint32_t pitch);
  virtual void processEvents();
  virtual void sleep(uint32_t duration);
  virtual uint32_t getTimeStamp();
  virtual void startAudio(AudioCallback callback, void *param);
  virtual void stopAudio();
  virtual uint32_t getOutputSampleRate();
  virtual void *addTimer(uint32_t delay, TimerCallback callback, void *param);
  virtual void removeTimer(void *timerId);
  virtual void *createMutex();
  virtual void destroyMutex(void *mutex);
  virtual void lockMutex(void *mutex);
  virtual void unlockMutex(void *mutex);
  virtual uint8_t* getOffScreenFramebuffer();

  VOID createWindow(BOOL fFullScreen);
  VOID writeEvent(ULONG ulEvent);
  ULONG readEvent();
  VOID scale3x(PBYTE pbDst, ULONG ulDstPitch, PBYTE pbSrc, ULONG ulSrcPitch,
               ULONG ulWidth, ULONG ulHeight);
  VOID audioCheckPause();
};

void OS2Stub::init(const char *title)
{
  ULONG			ulRC;
  DIVE_CAPS		DiveCaps = { 0 };
  FOURCC		fccFormats[100]	= { 0 };
  SEMRECORD		sSemRec;

  memset( &input, 0, sizeof(input) );
  ulEvWritePos = 0;
  ulEvReadPos = 0;
  hwndFrame = NULLHANDLE;
  fWinFocus = FALSE;
  fPause = FALSE;

  pszTitle = strdup( title );
  if ( pszTitle == NULL )
    error( "Not enough memory to store title" );

  pbOffscreen = (PBYTE)calloc( 1, SCREEN_W * SCREEN_H );
  if ( pbOffscreen == NULL )
    error( "Not enough memory for offscreen buffer." );

  hab = WinInitialize( 0 );
  hmq = WinCreateMsgQueue( hab, 0 );

  WinRegisterClass( hab, WIN_CLIENT_CLASS, ClientWndProc,
                    CS_SIZEREDRAW | CS_MOVENOTIFY, 0 );

  // Event semaphore for time interval at sleep()

  ulRC = DosCreateEventSem( NULL, &hevWakeup, DC_SEM_SHARED, FALSE );
  if ( ulRC != NO_ERROR )
    error( "Cannot create event semaphore for wakeup. Error code: %u", ulRC );

  // Timers multiple wait semaphore

  sSemRec.hsemCur = (HSEM)hevWakeup;
  sSemRec.ulUser = (ULONG)NULL;

  ulRC = DosCreateMuxWaitSem( NULL, &hmuxTimers, 1, &sSemRec, DCMW_WAIT_ANY );
  if ( ulRC != NO_ERROR )
    error( "Cannot create multiple wait semaphore. Error code: %u", ulRC );

  // DIVE initialization

  DiveCaps.pFormatData    = fccFormats;
  DiveCaps.ulFormatLength = 100;
  DiveCaps.ulStructLen    = sizeof(DIVE_CAPS);

  if ( DiveQueryCaps( &DiveCaps, DIVE_BUFFER_SCREEN ) )
    error( "DIVE routines cannot function in this system environment." );

  if ( DiveCaps.ulDepth < 8 )
    error( "Not enough screen colors to run DIVE. "
           "Must be at least 256 colors." );

  if ( DiveOpen( &hDive, FALSE, 0 ) )
    error( "Unable to open an instance of DIVE." );

  DosAllocMem( (PPVOID)&pbImgBuf, SCREEN_DIVE_W * SCREEN_DIVE_H,
               PAG_COMMIT | PAG_EXECUTE | PAG_READ | PAG_WRITE );

  if ( DiveAllocImageBuffer( hDive, &ulDiveBufferNumber, FOURCC_LUT8,
                             SCREEN_DIVE_W, SCREEN_DIVE_H, 0, pbImgBuf )
       != NO_ERROR )
    error( "DiveAllocImageBuffer() fail" );

  DiveSetDestinationPalette( hDive, 0, 16, 0 );

  // Create window.
  createWindow( FALSE );
}

void OS2Stub::destroy()
{
  WinDestroyWindow( hwndFrame );
  WinDestroyMsgQueue( hmq );
  WinTerminate( hab );
  DiveFreeImageBuffer( hDive, ulDiveBufferNumber );
  DiveClose( hDive );
  DosFreeMem( (PPVOID)&pbImgBuf );
  DosCloseEventSem( hevWakeup );
  DosCloseMuxWaitSem( hmuxTimers );
  free( pbOffscreen );
  free( pszTitle );
}

void OS2Stub::setPalette(uint8_t start, uint8_t numEnties, const uint8_t *buf)
{
  ULONG		ulIdx, ulComp;
  BYTE		bComp;
  BYTE		abPalette[16*4];
  PBYTE		pbPalette = (PBYTE)&abPalette;

  for( ulIdx = 0; ulIdx < numEnties; ulIdx++ )
  {
    for( ulComp = 0; ulComp < 3; ulComp++ )
    {
      bComp = buf[2-ulComp];
      *pbPalette = (bComp << 2) | (bComp & 3);
      pbPalette++;
    }
    buf += 3;
    *pbPalette = PC_RESERVED;
    pbPalette++;
  }

  DiveSetSourcePalette( hDive, start, numEnties, (char *)&abPalette );
}

void OS2Stub::copyRect(uint16_t x, uint16_t y, uint16_t width, uint16_t height, const uint8_t *buf, uint32_t pitch)
{
  PBYTE		pDest = pbOffscreen;
  ULONG		ulBufBPL = width / 2;
  ULONG		ulIdx;

  buf += y * pitch + x;
  pitch -= ulBufBPL;

  //For each line
  while( height-- )
  {
    //One byte gives us two pixels, we only need to iterate w/2 times.
    for( ulIdx = 0; ulIdx < ulBufBPL; ++ulIdx )
    {
      //Extract two palette indices from upper byte and lower byte.
      uint8_t p1 = *buf >> 4;
      uint8_t p2 = *(buf++) & 0xF;

      //Get the pixel value from the palette and write in in offScreen.
      *(pDest++) = p1;
      *(pDest++) = p2;
    }
    buf += pitch;
    pDest += SCREEN_W - width;
  }

  if ( pbImgBuf != NULL && hDive != NULLHANDLE )
  {
    scale3x( pbImgBuf, SCREEN_DIVE_W, pbOffscreen, SCREEN_W, SCREEN_W, SCREEN_H );
    DiveBlitImage( hDive, ulDiveBufferNumber, DIVE_BUFFER_SCREEN );
  }
}

void OS2Stub::processEvents()
{
  ULONG		ulEvent;

  while( TRUE )
  {
    ulEvent = readEvent();

    switch( ulEvent & 0xFFFF )
    {
      case EV_NULL:
        return;

      case EV_UP:
        input.dirMask |= PlayerInput::DIR_UP;
        break;

      case EV_KEYUP_UP:
        input.dirMask &= ~PlayerInput::DIR_UP;
        break;

      case EV_DOWN:
        input.dirMask |= PlayerInput::DIR_DOWN;
        break;

      case EV_KEYUP_DOWN:
        input.dirMask &= ~PlayerInput::DIR_DOWN;
        break;

      case EV_LEFT:
        input.dirMask |= PlayerInput::DIR_LEFT;
        break;

      case EV_KEYUP_LEFT:
        input.dirMask &= ~PlayerInput::DIR_LEFT;
        break;

      case EV_RIGHT:
        input.dirMask |= PlayerInput::DIR_RIGHT;
        break;

      case EV_KEYUP_RIGHT:
        input.dirMask &= ~PlayerInput::DIR_RIGHT;
        break;

      case EV_BUTTON:
        input.button = true;
        break;

      case EV_KEYUP_BUTTON:
        input.button = false;
        break;

      case EV_SAVE:
        input.save = true;
        break;

      case EV_LOAD:
        input.load = true;
        break;

      case EV_FASTMODE:
        input.fastMode = false;
        break;

      case EV_STATESLOT_INC:
        input.stateSlot = 1;
        break;

      case EV_STATESLOT_DEC:
        input.stateSlot = -1;
        break;

      case EV_CODE:
        input.code = true;
        break;
/*
      case EV_PAUSE:
        input.pause = true;
        break;
*/
      case EV_QUIT:
        input.quit = true;
        return;

      case EV_CHAR:
        input.lastChar = ulEvent >> 16;
        break;

      case EV_SWITCH_FULL_SCREEN:
        createWindow( !fFSMode );
        break;
    }
  }
}

void OS2Stub::sleep(uint32_t duration)
{
  QMSG		qmsg;
  ULONG		ulRC;
  HTIMER	hWakeupTimer;
  ULONG		ulCount;
  PSYSTIMER	pTimer;
  BOOL		fWakeup = FALSE;

  // Start timer on wakeup event semaphore
  ulRC = DosAsyncTimer( duration, (HSEM)hevWakeup, &hWakeupTimer );
  if ( ulRC != NO_ERROR )
    error( "Cannot create asynchronous timer for wakeup. Error code: %u", ulRC );

  do
  {
    // Process window messages

    if ( WinPeekMsg( hab, &qmsg, 0, 0, 0, PM_REMOVE ) )
    {
      if ( qmsg.msg == WM_QUIT )
      {
        writeEvent( EV_QUIT );
        break;
      }

      WinDispatchMsg( hab, &qmsg );
    }

    // Check timers

    ulRC = DosWaitMuxWaitSem( hmuxTimers, 1, (PULONG)&pTimer );

    if ( ulRC == ERROR_TIMEOUT )
      continue;

    if ( ulRC != NO_ERROR )
      error( "sleep(): DosWaitMuxWaitSem(), rc = %u", ulRC );

    if ( pTimer == NULL ) // Wakeup event semaphore
    {
      DosResetEventSem( hevWakeup, &ulCount );
      fWakeup = TRUE;
    }
    else
    {      
      pTimer->fCallback = TRUE;
      ulRC = pTimer->fnCallback( pTimer->ulDelay, pTimer->pParam );
      pTimer->fCallback = FALSE;

      if ( pTimer->hEv == NULLHANDLE )
      {
        // removeTimer() was called in callback function, the timer is not
        // destroyed yet. Destroy it completely now.
        removeTimer( (void *)pTimer );
      }
      else if ( ulRC == 0 )
      {
        ulRC = DosDeleteMuxWaitSem( hmuxTimers, (HSEM)pTimer->hEv );
        if ( ulRC != NO_ERROR )
          debug( DBG_INFO, "sleep(): DosDeleteMuxWaitSem(), rc = %u", ulRC );

        ulRC = DosStopTimer( pTimer->hTimer );
        if ( ulRC != NO_ERROR )
          debug( DBG_INFO, "sleep(): DosStopTimer(), rc = %u", ulRC );

        ulRC = DosCloseEventSem( pTimer->hEv );
        if ( ulRC != NO_ERROR )
          debug( DBG_INFO, "sleep(): DosCloseEventSem(), rc = %u", ulRC );

        pTimer->hTimer = NULLHANDLE;
        pTimer->hEv = NULLHANDLE;
      }
      else
      {
        DosResetEventSem( pTimer->hEv, &ulCount );

        ulRC = DosAsyncTimer( ulRC, (HSEM)pTimer->hEv, &pTimer->hTimer );
        if ( ulRC != NO_ERROR )
          error( "sleep(): DosAsyncTimer(), rc = %u", ulRC );
      }
    }
  }
  while( !fWakeup || !fWinFocus || fPause );

  DosStopTimer( hWakeupTimer );
}

uint32_t OS2Stub::getTimeStamp()
{
  ULONG		ulTime;

  DosQuerySysInfo( QSV_MS_COUNT, QSV_MS_COUNT, &ulTime, sizeof(ULONG) );
  return ulTime;
}

void OS2Stub::startAudio(AudioCallback callback, void *param)
{
#define _NUM_SOUND_BUFFERS	3
#define _SOUND_BUFFER_SIZE	2048
  MCI_AMP_OPEN_PARMS	sMCIAmpOpen;
  ULONG			ulRC, ulIdx;
  CHAR			acBuf[128];

  fnAudioCallback = callback;
  pAudioCallbackParam = param;

  memset( &sMCIAmpOpen, 0, sizeof(MCI_AMP_OPEN_PARMS) );
  sMCIAmpOpen.usDeviceID = 0;
  sMCIAmpOpen.pszDeviceType = (PSZ)MCI_DEVTYPE_AUDIO_AMPMIX;
  ulRC = mciSendCommand( 0, MCI_OPEN,
                         MCI_WAIT | MCI_OPEN_TYPE_ID | MCI_OPEN_SHAREABLE,
                         &sMCIAmpOpen,  0 );
  if ( ulRC != MCIERR_SUCCESS )
  {
    mciGetErrorString( ulRC, (PCHAR)&acBuf, sizeof(acBuf) );
    debug( DBG_INFO, "MCI_OPEN: %s", &acBuf );
    return;
  }

  usAudioDeviceId = sMCIAmpOpen.usDeviceID;

  memset( &sMCIMixSetup, 0, sizeof(MCI_MIXSETUP_PARMS) );
  sMCIMixSetup.ulBitsPerSample = 8;
  sMCIMixSetup.ulFormatTag     = MCI_WAVE_FORMAT_PCM;
  sMCIMixSetup.ulSamplesPerSec = SOUND_SAMPLE_RATE;
  sMCIMixSetup.ulChannels      = 1;
  sMCIMixSetup.ulFormatMode    = MCI_PLAY;
  sMCIMixSetup.ulDeviceType    = MCI_DEVTYPE_WAVEFORM_AUDIO;
  sMCIMixSetup.pmixEvent       = AudioEvent;

  ulRC = mciSendCommand( usAudioDeviceId, MCI_MIXSETUP,
                         MCI_WAIT | MCI_MIXSETUP_INIT,
                         &sMCIMixSetup, 0 );
  if ( ulRC != MCIERR_SUCCESS )
  {
    mciGetErrorString( ulRC, (PCHAR)&acBuf, sizeof(acBuf) );
    debug( DBG_INFO, "MCI_MIXSETUP: %s", &acBuf );
    return;
  }

  sMCIBuffer.ulNumBuffers = _NUM_SOUND_BUFFERS;
  sMCIBuffer.ulBufferSize = _SOUND_BUFFER_SIZE;
  sMCIBuffer.pBufList = (PMCI_MIX_BUFFER)malloc( sMCIBuffer.ulNumBuffers *
                                                 sizeof(MCI_MIX_BUFFER) );
  if ( sMCIBuffer.pBufList == NULL )
    error( "Not enough memory for DART buffers list" );

  ulRC = mciSendCommand( usAudioDeviceId, MCI_BUFFER,
                         MCI_WAIT | MCI_ALLOCATE_MEMORY, &sMCIBuffer, 0 );
  if ( ulRC != MCIERR_SUCCESS )
  {
    mciGetErrorString( ulRC, (PCHAR)&acBuf, sizeof(acBuf) );
    debug( DBG_INFO, "MCI_BUFFER: %s", &acBuf );
    return;
  }

  // Fill all device buffers with data.
  for( ulIdx = 0; ulIdx < sMCIBuffer.ulNumBuffers; ulIdx++)
  {
    ((PMCI_MIX_BUFFER)sMCIBuffer.pBufList)[ulIdx].ulFlags        = 0;
    ((PMCI_MIX_BUFFER)sMCIBuffer.pBufList)[ulIdx].ulBufferLength = sMCIBuffer.ulBufferSize;
    ((PMCI_MIX_BUFFER)sMCIBuffer.pBufList)[ulIdx].ulUserParm     = (ULONG)this;

    memset( ((PMCI_MIX_BUFFER)sMCIBuffer.pBufList)[ulIdx].pBuffer, 128,
            sMCIBuffer.ulBufferSize );
  }

  // Write buffers to kick off the amp mixer.
  ulRC = sMCIMixSetup.pmixWrite( sMCIMixSetup.ulMixHandle,
                                 (PMCI_MIX_BUFFER)sMCIBuffer.pBufList,
                                 sMCIBuffer.ulNumBuffers );
  if ( ulRC != MCIERR_SUCCESS )
    debug( DBG_INFO, "sMixSetupParms.pmixWrite(), rc = %u", ulRC );
}

void OS2Stub::stopAudio()
{
  ULONG			ulRC;
  CHAR			acBuf[128];
  MCI_GENERIC_PARMS	sMCIGeneric = { 0 };

  ulRC = mciSendCommand( usAudioDeviceId, MCI_STOP, MCI_WAIT,
                         &sMCIGeneric, 0 );
  if ( ulRC != MCIERR_SUCCESS )
  {
    mciGetErrorString( ulRC, (PCHAR)&acBuf, sizeof(acBuf) );
    debug( DBG_INFO, "MCI_STOP: %s", &acBuf );
  }

  ulRC = mciSendCommand( usAudioDeviceId, MCI_BUFFER,
                         MCI_WAIT | MCI_DEALLOCATE_MEMORY, &sMCIBuffer, 0 );
  if ( ulRC != MCIERR_SUCCESS )
  {
    mciGetErrorString( ulRC, (PCHAR)&acBuf, sizeof(acBuf) );
    debug( DBG_INFO, "MCI_BUFFER: %s", &acBuf );
  }

  ulRC = mciSendCommand( usAudioDeviceId, MCI_MIXSETUP,
                         MCI_WAIT | MCI_MIXSETUP_DEINIT, &sMCIMixSetup, 0 );
  if ( ulRC != MCIERR_SUCCESS )
  {
    mciGetErrorString( ulRC, (PCHAR)&acBuf, sizeof(acBuf) );
    debug( DBG_INFO, "MCI_MIXSETUP: %s", &acBuf );
  }

  ulRC = mciSendCommand( usAudioDeviceId, MCI_CLOSE, MCI_WAIT,
                         &sMCIGeneric, 0 );
  if ( ulRC != MCIERR_SUCCESS )
  {
    mciGetErrorString( ulRC, (PCHAR)&acBuf, sizeof(acBuf) );
    debug( DBG_INFO, "MCI_CLOSE: %s", &acBuf );
  }
}

uint32_t OS2Stub::getOutputSampleRate()
{
  return SOUND_SAMPLE_RATE;
}

void *OS2Stub::addTimer(uint32_t delay, TimerCallback callback, void *param)
{
  PSYSTIMER	pTimer = (PSYSTIMER)malloc( sizeof(SYSTIMER) );
  SEMRECORD	sSemRec;
  ULONG		ulRC;

  if ( pTimer == NULL )
    error( "Not enough memory for new timer" );

  pTimer->ulDelay = delay;
  pTimer->fnCallback = callback;
  pTimer->pParam = param;
  pTimer->fCallback = FALSE;

  ulRC = DosCreateEventSem( NULL, &pTimer->hEv, DC_SEM_SHARED, FALSE );
  if ( ulRC != NO_ERROR )
    error( "Cannot create event semaphore for timer. Error code: %u", ulRC );

  ulRC = DosAsyncTimer( delay, (HSEM)pTimer->hEv, &pTimer->hTimer );
  if ( ulRC != NO_ERROR )
    error( "Cannot create asynchronous timer. Error code: %u", ulRC );

  sSemRec.hsemCur = (HSEM)pTimer->hEv;
  sSemRec.ulUser = (ULONG)pTimer;
  ulRC = DosAddMuxWaitSem( hmuxTimers, &sSemRec );
  if ( ulRC != NO_ERROR )
    error( "Cannot add an event semaphore to the muxwait-semaphore list. "
           "Error code: %u", ulRC );

  debug( DBG_INFO, "Add timer: %p", pTimer );
  return pTimer;
}

void OS2Stub::removeTimer(void *timerId)
{
  PSYSTIMER	pTimer = (PSYSTIMER)timerId;
  ULONG		ulRC;

  if ( pTimer->hTimer != NULLHANDLE )
    DosStopTimer( pTimer->hTimer );

  if ( pTimer->hEv != NULLHANDLE )
  {
    ulRC = DosDeleteMuxWaitSem( hmuxTimers, (HSEM)pTimer->hEv );
    if ( ulRC != NO_ERROR )
      error( "Cannot delete an event semaphore from a muxwait-semaphore list. "
             "Error code: %u", ulRC );

    ulRC = DosCloseEventSem( pTimer->hEv );
    if ( ulRC != NO_ERROR )
      error( "Cannot close an event semaphore. Error code: %u", ulRC );
  }

  if ( pTimer->fCallback )
  {
    // removeTimer() called from callback function. Timer will be destroyed
    // completely after returning from callback function ( in sleep() ).
    pTimer->hTimer = NULLHANDLE;
    pTimer->hEv = NULLHANDLE;
  }
  else
  {
    free( pTimer );
    debug( DBG_INFO, "Remove timer: %p", pTimer );
  }
}

void *OS2Stub::createMutex()
{
  HMTX		hMtx;

  if ( DosCreateMutexSem( NULL, &hMtx, 0, FALSE ) != NO_ERROR )
    error( "DosCreateMutexSem() failed" );

  return (void *)hMtx;
}

void OS2Stub::destroyMutex(void *mutex)
{
  DosCloseMutexSem( (HMTX)mutex );
}

void OS2Stub::lockMutex(void *mutex)
{
  if ( DosRequestMutexSem( (HMTX)mutex, SEM_INDEFINITE_WAIT ) != NO_ERROR )
    error( "DosCreateMutexSem() failed" );
}

void OS2Stub::unlockMutex(void *mutex)
{
  DosReleaseMutexSem( (HMTX)mutex );
}

uint8_t* OS2Stub::getOffScreenFramebuffer()
{
  return NULL;
}


void OS2Stub::createWindow(BOOL fFullScreen)
{
  ULONG		flFrameFlags;
  HWND		hwndClient;
  ULONG		ulWinW = WinQuerySysValue( HWND_DESKTOP, SV_CXSCREEN );
  ULONG		ulWinH = WinQuerySysValue( HWND_DESKTOP, SV_CYSCREEN );

  if ( hwndFrame != NULLHANDLE )
    WinDestroyWindow( hwndFrame );

  if ( fFullScreen )
    flFrameFlags = FCF_TASKLIST;
  else
    flFrameFlags = FCF_TITLEBAR | FCF_DLGBORDER | FCF_MINBUTTON | FCF_SHELLPOSITION |
                   FCF_MAXBUTTON | FCF_TASKLIST | FCF_SYSMENU | FCF_SIZEBORDER;

  hwndFrame = WinCreateStdWindow( HWND_DESKTOP, 0, &flFrameFlags,
                WIN_CLIENT_CLASS, pszTitle, 0, 0, ID_APP_WINDOW,
                &hwndClient );
  if ( hwndFrame == NULLHANDLE )
    error( "WinCreateStdWindow() fail" );

  WinSetVisibleRegionNotify( hwndClient, TRUE );
  WinPostMsg( hwndFrame, WM_VRNENABLED, 0L, 0L );

  if ( !fFullScreen )
  {
    // Set client window width or height 2/3 of screen's size
    ulWinW = ulWinW * 2 / 3;
    ulWinH = ulWinH * 2 / 3;

    if ( ulWinW * SCREEN_H > ulWinH * SCREEN_W )
      ulWinW = ( ulWinH * SCREEN_W ) / SCREEN_H;
    else
      ulWinH = ( ulWinW * SCREEN_H ) / SCREEN_W;

    ulWinH += 2 * WinQuerySysValue( HWND_DESKTOP, SV_CYDLGFRAME ) +
              WinQuerySysValue( HWND_DESKTOP, SV_CYTITLEBAR );
    ulWinW += 2 * WinQuerySysValue( HWND_DESKTOP, SV_CXDLGFRAME );
  }

  WinSetWindowPos( hwndFrame, HWND_TOP, 0, 0, ulWinW, ulWinH,
                   SWP_SIZE | SWP_SHOW | SWP_ACTIVATE | SWP_ZORDER );

  fFSMode = fFullScreen;
}

VOID OS2Stub::writeEvent(ULONG ulEvent)
{
  ULONG		ulNewWritePos;

  if ( ulEvent == EV_QUIT )
    aulEvents[ulEvReadPos] = EV_QUIT;

  ulNewWritePos = ulEvWritePos + 1;
  if ( ulNewWritePos == INPUT_EVENTS_QUEUE_LEN )
    ulNewWritePos = 0;

  if ( ulNewWritePos == ulEvReadPos )
    debug( DBG_INFO, "Input events queue overflow." );
  else
  {
    aulEvents[ulEvWritePos] = ulEvent;
    ulEvWritePos = ulNewWritePos;
  }
}

ULONG OS2Stub::readEvent()
{
  ULONG		ulEvent;

  if ( ulEvWritePos == ulEvReadPos )
    ulEvent = EV_NULL;
  else
  {
    ulEvent = aulEvents[ulEvReadPos++];
    if ( ulEvReadPos == INPUT_EVENTS_QUEUE_LEN )
      ulEvReadPos = 0;
  }

  return ulEvent;
}

VOID OS2Stub::scale3x(PBYTE pbDst, ULONG ulDstPitch, PBYTE pbSrc,
                      ULONG ulSrcPitch, ULONG ulWidth, ULONG ulHeight)
{
  ULONG		ulLine, ulLinePixel;
  ULONG		aClip[9]; // source box 3x3 around source point

  for( ulLine = 0; ulLine < ulHeight; ulLine++ )
  {
    PBYTE	p = pbDst;

    aClip[1] = aClip[4] = aClip[7] = 0;
    aClip[2] = ulLine == 0 ? 0 : *(pbSrc - ulSrcPitch);
    aClip[5] = *(pbSrc + 1);
    aClip[8] = *(pbSrc + ulSrcPitch);

    for( ulLinePixel = 0; ulLinePixel < ulWidth; ulLinePixel++, p += 3 )
    {
      memcpy( &aClip[0], &aClip[1], sizeof(ULONG) * 8 );

      aClip[2] = ulLine == 0 || ulLinePixel == (ulWidth-1) ?
                   0 : *(pbSrc + ulLinePixel - ulSrcPitch + 1);
      aClip[5] = ulLinePixel == (ulWidth-1) ? 0 : *(pbSrc + ulLinePixel + 1);
      aClip[8] = ulLine == (ulHeight-1) || ulLinePixel == (ulWidth-1) ?
                   0 : *(pbSrc + ulLinePixel + ulSrcPitch + 1);

      if ( aClip[1] != aClip[7] && aClip[3] != aClip[5] )
      {
        p[0] = aClip[3] == aClip[1] ? aClip[3] : aClip[4];
        p[1] = (aClip[3] == aClip[1] && aClip[4] != aClip[2]) ||
                   (aClip[1] == aClip[5] && aClip[4] != aClip[0]) ?
                     aClip[1] : aClip[4];
        p[2] = aClip[1] == aClip[5] ? aClip[5] : aClip[4];
        p[ulDstPitch] = (aClip[3] == aClip[1] && aClip[4] != aClip[6]) ||
                          (aClip[3] == aClip[1] && aClip[4] != aClip[0]) ?
                            aClip[3] : aClip[4];
        p[ulDstPitch + 1] = aClip[4];
        p[ulDstPitch + 2] = (aClip[1] == aClip[5] && aClip[4] != aClip[8]) ||
                              (aClip[7] == aClip[5] && aClip[4] != aClip[2]) ?
                                aClip[5] : aClip[4];
        p[2 * ulDstPitch] = aClip[3] == aClip[7] ? aClip[3] : aClip[4];
        p[2 * ulDstPitch + 1] = (aClip[3] == aClip[7] && aClip[4] != aClip[8]) ||
                                  (aClip[7] == aClip[5] && aClip[4] != aClip[6]) ?
                                    aClip[7] : aClip[4];
        p[2 * ulDstPitch + 2] = aClip[7] == aClip[5] ? aClip[5] : aClip[4];
      }
      else
        p[0] = p[1] = p[2] = p[ulDstPitch] = p[ulDstPitch + 1] =
        p[ulDstPitch + 2] = p[2 * ulDstPitch] = p[2 * ulDstPitch + 1] =
        p[2 * ulDstPitch + 2] = aClip[4];
    }

    pbDst += ulDstPitch * 3;
    pbSrc += ulSrcPitch;
  }
}

VOID OS2Stub::audioCheckPause()
{
  MCI_GENERIC_PARMS	sMCIGeneric = { 0 };

  mciSendCommand( usAudioDeviceId,
                  fWinFocus && !fPause ? MCI_RESUME : MCI_PAUSE,
                  MCI_WAIT, &sMCIGeneric, 0 );
}


OS2Stub sysImpOS2;
System *stub = &sysImpOS2;


MRESULT EXPENTRY ClientWndProc(HWND hwnd, ULONG msg, MPARAM mp1, MPARAM mp2)
{
  static BOOL	fKeyAltGrafDown = FALSE;
  static HPS	hpsClient;
  HPS		hps;
  RECTL		sRectl;
  HRGN		hrgn;
  SIZEL		sizl;

  switch( msg )
  {
    case WM_CREATE:
      sizl.cx = sizl.cy = 0;

      hpsClient = GpiCreatePS( sysImpOS2.hab, WinOpenWindowDC( hwnd ),
                    &sizl, PU_PELS | GPIF_DEFAULT | GPIT_MICRO | GPIA_ASSOC );
      if ( hpsClient == NULLHANDLE )
        debug( DBG_INFO, "GpiCreatePS() fail" );
      break;

    case WM_DESTROY:
      if ( hpsClient != NULLHANDLE )
        GpiDestroyPS( hpsClient );
      break;

    case WM_PAINT:
      hps = WinBeginPaint( hwnd, NULLHANDLE, &sRectl );
      WinFillRect( hps, &sRectl, CLR_BLACK );
      WinEndPaint( hps );
      DiveBlitImage( sysImpOS2.hDive, sysImpOS2.ulDiveBufferNumber,
                     DIVE_BUFFER_SCREEN );
      return 0;

    case WM_REALIZEPALETTE:
      DiveSetDestinationPalette( sysImpOS2.hDive, 0, 256, 0 );
      break;

    case WM_VRNDISABLED:
      DiveSetupBlitter( sysImpOS2.hDive, 0 );
      break;

    case WM_MOVE:
    case WM_SIZE:
      WinPostMsg( hwnd, WM_VRNENABLED, 0L, 0L);
      WinPostMsg( hwnd, WM_PAINT, 0L, 0L);
      break;

    case WM_VRNENABLED:
      if ( hpsClient == NULLHANDLE )
        break;

      hrgn = GpiCreateRegion( hpsClient, 0L, NULL );
      if ( hrgn )
      {
        RGNRECT            rgnCtl;         // Processing control structure
        POINTL             pointl;         // Point to offset from Desktop
        RECTL              rcls[50];       // Rectangle coordinates
        SWP                swp;            // Window position
        SETUP_BLITTER      SetupBlitter;   // structure for DiveSetupBlitter

        WinQueryVisibleRegion( hwnd, hrgn );
        rgnCtl.ircStart     = 0;
        rgnCtl.crc          = 50;
        rgnCtl.ulDirection  = 1;

        // Get the all rectangles
        if ( GpiQueryRegionRects( hpsClient, hrgn, NULL, &rgnCtl, rcls ) )
        {
          // Now find the window position and size, relative to parent.
          WinQueryWindowPos( hwnd, &swp );

          // Convert the point to offset from desktop lower left.
          pointl.x = 0;
          pointl.y = 0;
          WinMapWindowPoints( hwnd, HWND_DESKTOP, &pointl, 1 );

          // Tell DIVE about the new settings.
          SetupBlitter.ulStructLen = sizeof(SETUP_BLITTER);
          SetupBlitter.fccSrcColorFormat = FOURCC_LUT8;
          SetupBlitter.ulSrcWidth   = OS2Stub::SCREEN_DIVE_W;
          SetupBlitter.ulSrcHeight  = OS2Stub::SCREEN_DIVE_H;
          SetupBlitter.ulSrcPosX    = 0;
          SetupBlitter.ulSrcPosY    = 0;
          SetupBlitter.fInvert      = FALSE;
          SetupBlitter.ulDitherType = 1;
          SetupBlitter.fccDstColorFormat = FOURCC_SCRN;

          SetupBlitter.lScreenPosX       = pointl.x;
          SetupBlitter.lScreenPosY       = pointl.y;
          SetupBlitter.ulNumDstRects     = rgnCtl.crcReturned;
          SetupBlitter.pVisDstRects      = rcls;

          // Fit to window, keep proportional.
          if ( swp.cx * OS2Stub::SCREEN_DIVE_H > swp.cy * OS2Stub::SCREEN_DIVE_W )
          {
            SetupBlitter.ulDstHeight = swp.cy;
            SetupBlitter.ulDstWidth = ( swp.cy * OS2Stub::SCREEN_DIVE_W ) / OS2Stub::SCREEN_DIVE_H;
          }
          else
          {
            SetupBlitter.ulDstWidth = swp.cx;
            SetupBlitter.ulDstHeight = ( swp.cx * OS2Stub::SCREEN_DIVE_H ) / OS2Stub::SCREEN_DIVE_W;
          }
          SetupBlitter.lDstPosX = ( swp.cx - SetupBlitter.ulDstWidth ) / 2;
          SetupBlitter.lDstPosY = ( swp.cy - SetupBlitter.ulDstHeight ) / 2;

          if ( DiveSetupBlitter( sysImpOS2.hDive, &SetupBlitter ) !=
               DIVE_SUCCESS )
            debug( DBG_INFO, "DiveSetupBlitter() failed" );
        }
        else
          DiveSetupBlitter( sysImpOS2.hDive, 0 );

        GpiDestroyRegion( hpsClient, hrgn );
      }
      break;

    case WM_SETFOCUS:
      sysImpOS2.fWinFocus = SHORT1FROMMP( mp2 ) != 0;
      sysImpOS2.audioCheckPause();
      if ( sysImpOS2.fWinFocus )
        WinPostMsg( hwnd, WM_PAINT, 0L, 0L);
      break;

    case WM_CHAR:
      {
        USHORT	usFlags = SHORT1FROMMP( mp1 );
        BOOL	fVirtualKey = (usFlags & KC_VIRTUALKEY) != 0;
        UCHAR	ucScanCode = CHAR4FROMMP( mp1 );
        USHORT	usVKCode = SHORT2FROMMP( mp2 );
        BOOL	fKeyAltDown = fKeyAltGrafDown ||
                                ( (usFlags & KC_ALT) != 0 );

        if ( ( (usFlags & KC_VIRTUALKEY) != 0 ) && ( usVKCode == VK_ALTGRAF ) )
        {
          fKeyAltGrafDown = (usFlags & KC_KEYUP) == 0;
          break;
        }

        if ( (usFlags & KC_KEYUP) != 0 )
        {
          switch( ucScanCode )
          {
            case 0x4D:		// Right
            case 0x64:
              sysImpOS2.writeEvent( OS2Stub::EV_KEYUP_RIGHT );
              break;

            case 0x48:		// Up
            case 0x61:
              sysImpOS2.writeEvent( OS2Stub::EV_KEYUP_UP );
              break;

            case 0x4B:		// Left
            case 0x63:
              sysImpOS2.writeEvent( OS2Stub::EV_KEYUP_LEFT );
              break;

            case 0x50:		// Down
            case 0x66:
              sysImpOS2.writeEvent( OS2Stub::EV_KEYUP_DOWN );
              break;

            case 0x39:		// Space
            case 0x1C:		// New Line
            case 0x5A:		// Enter
              sysImpOS2.writeEvent( OS2Stub::EV_KEYUP_BUTTON );
              break;
          }
        }
        else
        {
          if ( ( fVirtualKey && usVKCode == VK_F3 ) ||
               ( fKeyAltDown &&					  // Alt +
                 ( ucScanCode == 0x1C || ucScanCode == 0x5A ) ) ) // Enter
          {
            sysImpOS2.writeEvent( OS2Stub::EV_SWITCH_FULL_SCREEN );
            break;
          }

          if ( fKeyAltDown && ucScanCode == 0x2D ) // Alt + 'X'
          {
            sysImpOS2.writeEvent( OS2Stub::EV_QUIT );
            break;            
          }

          if ( (usFlags & KC_CTRL) != 0 )
          {
            switch( ucScanCode )
            {
              case 0x1F:		// 'S'
                sysImpOS2.writeEvent( OS2Stub::EV_SAVE );
                break;

              case 0x26:		// 'L'
                sysImpOS2.writeEvent( OS2Stub::EV_LOAD );
                break;

              case 0x21:		// 'F'
                sysImpOS2.writeEvent( OS2Stub::EV_FASTMODE );
                break;

              case 0x4E:		// numpad '+'
                sysImpOS2.writeEvent( OS2Stub::EV_STATESLOT_INC );
                break;

              case 0x4A:		// numpad '-'
                sysImpOS2.writeEvent( OS2Stub::EV_STATESLOT_DEC );
                break;
            }
          }
          else
          {
            switch( ucScanCode )
            {
              case 0x4D:		// Right
              case 0x64:
                sysImpOS2.writeEvent( OS2Stub::EV_RIGHT );
                break;

              case 0x48:		// Up
              case 0x61:
                sysImpOS2.writeEvent( OS2Stub::EV_UP );
                break;

              case 0x4B:		// Left
              case 0x63:
                sysImpOS2.writeEvent( OS2Stub::EV_LEFT );
                break;

              case 0x50:		// Down
              case 0x66:
                sysImpOS2.writeEvent( OS2Stub::EV_DOWN );
                break;

              case 0x39:		// Space
              case 0x1C:		// New Line
              case 0x5A:		// Enter
                sysImpOS2.writeEvent( OS2Stub::EV_BUTTON );
                break;

              case 0x2E:		// 'C'
                sysImpOS2.writeEvent( OS2Stub::EV_CODE );
                break;

              case 0x19:		// 'P'
                sysImpOS2.fPause = !sysImpOS2.fPause;
                sysImpOS2.audioCheckPause();
//                sysImpOS2.writeEvent( OS2Stub::EV_PAUSE );
                break;

              default: // hm... what about C and P ?!
                if ( (usFlags & KC_CHAR) != 0 )
                  sysImpOS2.writeEvent( OS2Stub::EV_CHAR | SHORT1FROMMP( mp2 ) << 16 );
            }
          }
        }
      } // WM_CHAR
  }

  return WinDefWindowProc( hwnd, msg, mp1, mp2 );
}

LONG APIENTRY AudioEvent(ULONG ulStatus, PMCI_MIX_BUFFER pBuffer, ULONG ulFlags)
{
  sysImpOS2.fnAudioCallback( sysImpOS2.pAudioCallbackParam,
                            (PUCHAR)pBuffer->pBuffer,
                            pBuffer->ulBufferLength );
  sysImpOS2.sMCIMixSetup.pmixWrite( sysImpOS2.sMCIMixSetup.ulMixHandle,
                                    pBuffer, 1 );
  return TRUE;
}
