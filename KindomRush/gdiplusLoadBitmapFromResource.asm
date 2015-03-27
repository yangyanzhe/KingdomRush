include gdiplus.inc

IFNDEF GDIPLUS_LOADBITMAPFROMRESOURCE_INC
GDIPLUS_LOADBITMAPFROMRESOURCE_INC equ <1>

;
; Load JPG/PNG from resource using GDI+
;   Actually, this function can load any image format supported by GDI+
;
; by: Chris Vega
;

gdiplusLoadBitmapFromResource proc hInst:DWORD, lpName:DWORD, lpType:DWORD, pBitmapFromStream:DWORD

	local rcRes:HRSRC
	local hResData:HRSRC
	local pResData:HANDLE
	local sizeOfRes:DWORD
	local hbuffer:HANDLE
	local pbuffer:DWORD
	local pIStream:DWORD
	local hIStream:DWORD

	; ------------------------------------------------------------------
	; STEP 1: Find the resource
	; ------------------------------------------------------------------
	invoke	FindResource, hInst, lpName, lpType
	or 		eax, eax
	jnz		@f
	jmp		gdiplusLoadBitmapFromResource@Close
@@:	mov		rcRes, eax
	
	; ------------------------------------------------------------------
	; STEP 2: Load the resource
	; ------------------------------------------------------------------
	invoke	LoadResource, hInst, rcRes
	or		eax, eax
	jnz		@f
	ret		; Resource was not loaded
@@:	mov		hResData, eax

	; ------------------------------------------------------------------
	; STEP 3: Create a stream to contain our loaded resource
	; ------------------------------------------------------------------
	invoke	SizeofResource, hInst, rcRes
	or		eax, eax
	jnz		@f
	jmp		gdiplusLoadBitmapFromResource@Close
@@:	mov		sizeOfRes, eax
	
	invoke	LockResource, hResData
	or		eax, eax
	jnz		@f
	jmp		gdiplusLoadBitmapFromResource@Close
@@:	mov		pResData, eax

	invoke	GlobalAlloc, GMEM_MOVEABLE, sizeOfRes
	or		eax, eax
	jnz		@f
	jmp		gdiplusLoadBitmapFromResource@Close
@@:	mov		hbuffer, eax

	invoke	GlobalLock, hbuffer
	mov		pbuffer, eax
	
	invoke	RtlMoveMemory, pbuffer, hResData, sizeOfRes
	invoke	CreateStreamOnHGlobal, pbuffer, FALSE, addr pIStream
	or		eax, eax
	jz		@f
	jmp		gdiplusLoadBitmapFromResource@Close
@@:	

	; ------------------------------------------------------------------
	; STEP 4: Create an image object from stream
	; ------------------------------------------------------------------
	invoke	GdipCreateBitmapFromStream, pIStream, pBitmapFromStream
	
	; ------------------------------------------------------------------
	; STEP 5: Free all used locks and resources
	; ------------------------------------------------------------------
	invoke	GetHGlobalFromStream, pIStream, addr hIStream
	invoke	GlobalFree, hIStream
	invoke	GlobalUnlock, hbuffer
	invoke	GlobalFree, hbuffer
	
gdiplusLoadBitmapFromResource@Close:
	ret
gdiplusLoadBitmapFromResource endp

ENDIF