/* ANCHOR Studio.exe - starts ANCHOR Studio with the R runtime shipped next to it.
 *   <folder>\ANCHOR Studio.exe   (this program)
 *   <folder>\runtime\            (R 4.3 and all R packages, Windows build from conda-forge)
 *   <folder>\app\                (the Shiny app; app\launch.R opens the window)
 * No console window. A small splash shows until the app window is open.
 * Log: %LOCALAPPDATA%\ANCHOR Studio\anchor.log
 * ANCHOR Studio by A. Mamdouh Kastour (A. Mamdouh Kastour's Scholarly Anchor). Author: Amira Mamdouh Kastour. Licence: GPL-3.
 */
#ifndef UNICODE
#define UNICODE
#endif
#ifndef _UNICODE
#define _UNICODE
#endif
#include <windows.h>
#include <shlobj.h>
#include <wchar.h>

static wchar_t g_ready[MAX_PATH];
static HWND g_splash;

static LRESULT CALLBACK SplashProc(HWND h, UINT m, WPARAM w, LPARAM l) {
  switch (m) {
  case WM_PAINT: {
    PAINTSTRUCT ps; HDC dc = BeginPaint(h, &ps); RECT r; GetClientRect(h, &r);
    HBRUSH bg = CreateSolidBrush(RGB(20, 33, 61)); FillRect(dc, &r, bg); DeleteObject(bg);
    RECT bar = { r.left + 40, r.bottom - 34, r.right - 40, r.bottom - 30 };
    HBRUSH ac = CreateSolidBrush(RGB(180, 86, 26)); FillRect(dc, &bar, ac); DeleteObject(ac);
    SetBkMode(dc, TRANSPARENT); SetTextColor(dc, RGB(251, 250, 246));
    HFONT big = CreateFontW(34, 0, 0, 0, FW_SEMIBOLD, 0, 0, 0, DEFAULT_CHARSET, 0, 0, CLEARTYPE_QUALITY, 0, L"Georgia");
    HFONT small = CreateFontW(17, 0, 0, 0, FW_NORMAL, 0, 0, 0, DEFAULT_CHARSET, 0, 0, CLEARTYPE_QUALITY, 0, L"Segoe UI");
    HFONT by = CreateFontW(16, 0, 0, 0, FW_NORMAL, TRUE, 0, 0, DEFAULT_CHARSET, 0, 0, CLEARTYPE_QUALITY, 0, L"Georgia");
    RECT t = { r.left, r.top + 22, r.right, r.top + 64 };
    SelectObject(dc, big); DrawTextW(dc, L"ANCHOR Studio", -1, &t, DT_CENTER | DT_SINGLELINE);
    RECT b = { r.left, r.top + 66, r.right, r.top + 88 };
    SetTextColor(dc, RGB(232, 160, 110)); SelectObject(dc, by);
    DrawTextW(dc, L"by A. Mamdouh Kastour \x00b7 Scholarly Anchor", -1, &b, DT_CENTER | DT_SINGLELINE);
    RECT s = { r.left, r.top + 100, r.right, r.top + 124 };
    SetTextColor(dc, RGB(251, 250, 246)); SelectObject(dc, small); DrawTextW(dc, L"Starting\x2026 this takes a few seconds", -1, &s, DT_CENTER | DT_SINGLELINE);
    DeleteObject(big); DeleteObject(small); DeleteObject(by); EndPaint(h, &ps); return 0; }
  case WM_TIMER:
    if (GetFileAttributesW(g_ready) != INVALID_FILE_ATTRIBUTES) { KillTimer(h, 1); DestroyWindow(h); }
    return 0;
  case WM_DESTROY: g_splash = NULL; return 0;
  }
  return DefWindowProcW(h, m, w, l);
}

static void show_error(const wchar_t *msg) {
  MessageBoxW(NULL, msg, L"ANCHOR Studio", MB_OK | MB_ICONERROR);
}

/* last part of the log (UTF-8 or ANSI text) for error messages */
static void log_tail(const wchar_t *log, wchar_t *out, int outlen) {
  out[0] = 0;
  HANDLE h = CreateFileW(log, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h == INVALID_HANDLE_VALUE) return;
  LARGE_INTEGER sz; GetFileSizeEx(h, &sz);
  DWORD want = 1400, got = 0; char buf[1500];
  if (sz.QuadPart > want) { LARGE_INTEGER off; off.QuadPart = sz.QuadPart - want; SetFilePointerEx(h, off, NULL, FILE_BEGIN); }
  ReadFile(h, buf, want, &got, NULL); CloseHandle(h); buf[got] = 0;
  int n = MultiByteToWideChar(CP_UTF8, 0, buf, (int)got, out, outlen - 1);
  if (n <= 0) n = MultiByteToWideChar(CP_ACP, 0, buf, (int)got, out, outlen - 1);
  out[n > 0 ? n : 0] = 0;
}

int WINAPI wWinMain(HINSTANCE inst, HINSTANCE prev, PWSTR cmd, int show) {
  (void)prev; (void)show;
  wchar_t dir[MAX_PATH], rhome[MAX_PATH], rscript[MAX_PATH], app[MAX_PATH], logdir[MAX_PATH], log[MAX_PATH];
  GetModuleFileNameW(NULL, dir, MAX_PATH);
  wchar_t *slash = wcsrchr(dir, L'\\'); if (slash) *slash = 0;

  swprintf(rhome, MAX_PATH, L"%ls\\runtime\\lib\\R", dir);
  swprintf(rscript, MAX_PATH, L"%ls\\bin\\x64\\Rscript.exe", rhome);
  swprintf(app, MAX_PATH, L"%ls\\app\\launch.R", dir);
  if (GetFileAttributesW(rscript) == INVALID_FILE_ATTRIBUTES || GetFileAttributesW(app) == INVALID_FILE_ATTRIBUTES) {
    show_error(L"ANCHOR Studio is incomplete: the 'runtime' or 'app' folder next to this program is missing.\n\nReinstall ANCHOR Studio, or unzip the whole portable folder before starting it.");
    return 1;
  }

  /* log and ready-marker in %LOCALAPPDATA%\ANCHOR Studio */
  if (FAILED(SHGetFolderPathW(NULL, CSIDL_LOCAL_APPDATA | CSIDL_FLAG_CREATE, NULL, 0, logdir))) GetTempPathW(MAX_PATH, logdir);
  wcscat(logdir, L"\\ANCHOR Studio"); CreateDirectoryW(logdir, NULL);
  swprintf(log, MAX_PATH, L"%ls\\anchor.log", logdir);
  swprintf(g_ready, MAX_PATH, L"%ls\\ready.flag", logdir);
  DeleteFileW(g_ready);

  /* environment for the bundled R (what conda's activation would set) */
  static wchar_t path[32767], old[32767];
  GetEnvironmentVariableW(L"PATH", old, 32767);
  swprintf(path, 32767,
    L"%ls\\runtime;%ls\\runtime\\Library\\mingw-w64\\bin;%ls\\runtime\\Library\\usr\\bin;%ls\\runtime\\Library\\bin;%ls\\runtime\\Scripts;%ls\\bin\\x64;%ls",
    dir, dir, dir, dir, dir, rhome, old);
  SetEnvironmentVariableW(L"PATH", path);
  SetEnvironmentVariableW(L"R_HOME", rhome);
  SetEnvironmentVariableW(L"R_LIBS", NULL);
  SetEnvironmentVariableW(L"R_LIBS_USER", L"NULL");
  SetEnvironmentVariableW(L"R_LIBS_SITE", L"NULL");
  SetEnvironmentVariableW(L"ANCHOR_READY_FILE", g_ready);

  /* a project file double-clicked in Explorer arrives as the first argument: hand it to the app */
  static wchar_t proj[32767]; proj[0] = 0;
  if (cmd && *cmd) {
    const wchar_t *s = cmd; while (*s == L' ') s++;
    if (*s == L'"') { s++; const wchar_t *e = wcschr(s, L'"'); size_t n = e ? (size_t)(e - s) : wcslen(s); if (n > 32766) n = 32766; wcsncpy(proj, s, n); proj[n] = 0; }
    else { wcsncpy(proj, s, 32766); proj[32766] = 0; size_t n = wcslen(proj); while (n && proj[n - 1] == L' ') proj[--n] = 0; }
  }
  SetEnvironmentVariableW(L"ANCHOR_OPEN", (proj[0] && GetFileAttributesW(proj) != INVALID_FILE_ATTRIBUTES) ? proj : NULL);

  /* start R hidden, output to the log */
  SECURITY_ATTRIBUTES sa = { sizeof(sa), NULL, TRUE };
  HANDLE lf = CreateFileW(log, GENERIC_WRITE, FILE_SHARE_READ, &sa, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  STARTUPINFOW si; ZeroMemory(&si, sizeof(si)); si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES; si.hStdOutput = lf; si.hStdError = lf; si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
  PROCESS_INFORMATION pi; ZeroMemory(&pi, sizeof(pi));
  wchar_t cmdline[4 * MAX_PATH];
  swprintf(cmdline, 4 * MAX_PATH, L"\"%ls\" --vanilla \"%ls\"", rscript, app);
  if (!CreateProcessW(rscript, cmdline, NULL, NULL, TRUE, CREATE_NO_WINDOW, NULL, dir, &si, &pi)) {
    show_error(L"ANCHOR Studio could not start its R runtime. Reinstalling usually fixes this.");
    return 1;
  }
  if (lf != INVALID_HANDLE_VALUE) CloseHandle(lf);

  /* splash until launch.R writes the ready marker (or 3 minutes pass) */
  WNDCLASSW wc; ZeroMemory(&wc, sizeof(wc));
  wc.lpfnWndProc = SplashProc; wc.hInstance = inst; wc.lpszClassName = L"AnchorSplash";
  wc.hCursor = LoadCursor(NULL, IDC_APPSTARTING); wc.hIcon = LoadIconW(inst, MAKEINTRESOURCEW(1));
  RegisterClassW(&wc);
  int W = 420, H = 170, X = (GetSystemMetrics(SM_CXSCREEN) - W) / 2, Y = (GetSystemMetrics(SM_CYSCREEN) - H) / 2;
  g_splash = CreateWindowExW(WS_EX_TOOLWINDOW | WS_EX_TOPMOST, L"AnchorSplash", L"ANCHOR Studio", WS_POPUP | WS_VISIBLE, X, Y, W, H, NULL, NULL, inst, NULL);
  SetTimer(g_splash, 1, 400, NULL);
  DWORD start = GetTickCount();
  MSG msg; int slow_told = 0;
  while (g_splash) {
    while (PeekMessageW(&msg, NULL, 0, 0, PM_REMOVE)) { TranslateMessage(&msg); DispatchMessageW(&msg); }
    if (WaitForSingleObject(pi.hProcess, 50) == WAIT_OBJECT_0) { if (g_splash) DestroyWindow(g_splash); break; }
    if (GetTickCount() - start > 180000 && g_splash) { DestroyWindow(g_splash); slow_told = 1; }
  }
  int was_ready = GetFileAttributesW(g_ready) != INVALID_FILE_ATTRIBUTES;
  if (slow_told && !was_ready) {
    wchar_t m[2 * MAX_PATH];
    swprintf(m, 2 * MAX_PATH, L"ANCHOR Studio is taking unusually long to start. It keeps trying in the background.\n\nIf no window opens, start \"ANCHOR Studio - troubleshoot\" from the Start menu or send this file:\n%ls", log);
    MessageBoxW(NULL, m, L"ANCHOR Studio", MB_OK | MB_ICONWARNING);
  }

  /* wait for the app to finish; report a crash, or an exit before the window ever opened */
  WaitForSingleObject(pi.hProcess, INFINITE);
  DWORD code = 0; GetExitCodeProcess(pi.hProcess, &code);
  CloseHandle(pi.hProcess); CloseHandle(pi.hThread);
  was_ready = was_ready || GetFileAttributesW(g_ready) != INVALID_FILE_ATTRIBUTES;
  if (code != 0 || !was_ready) {
    static wchar_t tail[1600], m[2400];
    log_tail(log, tail, 1600);
    swprintf(m, 2400, L"ANCHOR Studio stopped before its window opened (code %lu).\n\nLast lines of the start-up log:\n\n%ls\n\nFull log: %ls\nFor a step-by-step view, start \"ANCHOR Studio - troubleshoot\" from the Start menu.", code, tail, log);
    show_error(m);
    return code ? (int)code : 2;
  }
  return 0;
}
