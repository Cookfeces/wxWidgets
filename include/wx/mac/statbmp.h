/////////////////////////////////////////////////////////////////////////////
// Name:        statbmp.h
// Purpose:     wxStaticBitmap class
// Author:      AUTHOR
// Modified by:
// Created:     ??/??/98
// RCS-ID:      $Id$
// Copyright:   (c) AUTHOR
// Licence:   	wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#ifndef _WX_STATBMP_H_
#define _WX_STATBMP_H_

#ifdef __GNUG__
#pragma interface "statbmp.h"
#endif

#include "wx/icon.h"

class WXDLLEXPORT wxStaticBitmap: public wxStaticBitmapBase
{
  DECLARE_DYNAMIC_CLASS(wxStaticBitmap)
 public:
  inline wxStaticBitmap() { }

  inline wxStaticBitmap(wxWindow *parent, wxWindowID id,
           const wxBitmap& label,
           const wxPoint& pos = wxDefaultPosition,
           const wxSize& size = wxDefaultSize,
           long style = 0,
           const wxString& name = wxStaticBitmapNameStr)
  {
    Create(parent, id, label, pos, size, style, name);
  }

  bool Create(wxWindow *parent, wxWindowID id,
           const wxBitmap& label,
           const wxPoint& pos = wxDefaultPosition,
           const wxSize& size = wxDefaultSize,
           long style = 0,
           const wxString& name = wxStaticBitmapNameStr);

  virtual void SetBitmap(const wxBitmap& bitmap);

  virtual void Command(wxCommandEvent& WXUNUSED(event)) {};
  virtual void ProcessCommand(wxCommandEvent& WXUNUSED(event)) {};
  virtual void OnPaint( wxPaintEvent &event ) ;
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);

  wxBitmap GetBitmap() const { return m_bitmap; }
  wxIcon GetIcon() const
      {
	  // icons and bitmaps are really the same thing in wxMac
	  return (const wxIcon &)m_bitmap;
      }
  void  SetIcon(const wxIcon& icon) { SetBitmap( (const wxBitmap &)icon ) ; }

  // overriden base class virtuals
  virtual bool AcceptsFocus() const { return FALSE; }
  virtual  wxSize DoGetBestSize() const ;

 protected:
  wxBitmap m_bitmap;
	DECLARE_EVENT_TABLE() 
};

#endif
    // _WX_STATBMP_H_
