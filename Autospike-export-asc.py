# written by matthias.schott@uni-bayreuth.de
# Creative Commons  BY-NC-SA 4.0
# Attribution-NonCommercial-ShareAlike

import time
import pyautogui
import win32gui
import win32con
import win32process
import ctypes


#### to identify the window name
def getWindowNames( hwnd, ctx ):
    if win32gui.IsWindowVisible( hwnd ):
        print ( hex( hwnd ), win32gui.GetWindowText( hwnd ) )
win32gui.EnumWindows(getWindowNames, None )

### if you want sound
SOUND = True
if SOUND:
    import winsound
    frequency = 400
    duration = 500
###

def find_window_by_title(wildcard):
    """
    Sucht alle sichtbaren Fenster und gibt das erste zurück, dessen Titel den Suchbegriff (ohne Groß-/Kleinschreibung) enthält.
    """
    def enum_handler(hwnd, results):
        if win32gui.IsWindowVisible(hwnd):
            title = win32gui.GetWindowText(hwnd)
            if wildcard.lower() in title.lower():
                results.append(hwnd)
    found_windows = []
    win32gui.EnumWindows(enum_handler, found_windows)
    return found_windows[0] if found_windows else None

def bring_window_to_foreground(hwnd):
    """
    Bringt das Fenster mit dem übergebenen Handle in den Vordergrund.
    Falls das Fenster minimiert ist, wird es wiederhergestellt.
    Dabei wird AttachThreadInput genutzt, um Windows die Änderung zu ermöglichen.
    """
    if hwnd is None:
        return False

    # Falls das Fenster minimiert ist, wiederherstellen
    if win32gui.IsIconic(hwnd):
        win32gui.ShowWindow(hwnd, win32con.SW_RESTORE)
    
    # Aktuelles Vordergrundfenster und zugehörige Thread IDs ermitteln
    fg_hwnd = win32gui.GetForegroundWindow()
    current_thread = win32process.GetWindowThreadProcessId(fg_hwnd)[0]
    target_thread = win32process.GetWindowThreadProcessId(hwnd)[0]
    
    # Eingabe der Threads verbinden
    ctypes.windll.user32.AttachThreadInput(target_thread, current_thread, True)
    try:
        win32gui.BringWindowToTop(hwnd)
        win32gui.SetForegroundWindow(hwnd)
    except Exception as e:
        print("Fehler beim Setzen des Vordergrundfensters:", e)
        return False
    finally:
        # Verbindung der Thread-Eingaben wieder lösen
        ctypes.windll.user32.AttachThreadInput(target_thread, current_thread, False)
    return True
  
def wait_for_window(title, timeout=10):
    """
    Wartet (bis zu 'timeout'-Sekunden) darauf, dass ein Fenster mit dem
    angegebenen Titel erscheint. Gibt den Fenster-Handle zurück, wenn es gefunden wurde,
    oder None, wenn die Wartezeit abläuft.
    """
    start_time = time.time()
    while time.time() - start_time < timeout:
        hwnd = find_window_by_title(title)
        if hwnd is not None:
            return hwnd
        time.sleep(0.1)
    return None
  
def wait_until_program_responsive(hwnd, timeout=300):
    """
    Wartet, bis das Fenster mit dem übergebenen Handle wieder reagiert.
    IsHungAppWindow() gibt einen Wert ungleich 0 zurück, wenn das Fenster "gehangen" ist.
    """
    start_time = time.time()
    while time.time() - start_time < timeout:
        # Ist das Fenster nicht gehangen (d.h. reagiert)?
        if ctypes.windll.user32.IsHungAppWindow(hwnd) == 0:
            return True
        time.sleep(0.5)
    return False

def main():
    # Benutzerabfragen
    filename = input("Bitte Dateiname eingeben: ")
    num_traces_input = input("Wie viele Traces sollen exportiert werden? ")
    try:
        num_traces = int(num_traces_input)
    except ValueError:
        print("Ungültige Eingabe. Es wird 1 Trace exportiert.")
        num_traces = 1

    # Fenster mit "Autospike" im Titel finden und in den Vordergrund bringen
    hwnd = find_window_by_title("AutoSpike")
    if hwnd is None:
        print("Kein Fenster gefunden, dessen Titel 'Autospike' enthält!")
        return
    print("Try bringing window to focus")
    if not bring_window_to_foreground(hwnd):
        print("Konnte das Fenster nicht in den Vordergrund bringen.")
        return
    bring_window_to_foreground(hwnd)
    time.sleep(5)  # 1 Sekunde Pause
    print("is it in focus?")
    # Exportablauf für jeden Trace
    for i in range(1, num_traces + 1):
        # Alt Gr wird als Kombination von Strg+Alt emuliert
        pyautogui.hotkey( 'alt', 'f')
        time.sleep(0.1)
        pyautogui.press('e')
        time.sleep(0.1)
        pyautogui.press('a')
        time.sleep(0.1)
        
        # Viermal Pfeil nach unten drücken
        for _ in range((i*2)-1):
            pyautogui.press('down')
            time.sleep(1)
        time.sleep(5)
        pyautogui.press('enter')
        
        # Einmal Pfeil nach unten und Enter drücken
        # pyautogui.press('down')
        # pyautogui.press('enter')
        
        # Dateiname mit Endung "-<TraceNr>" eingeben und mit Enter bestätigen
        time.sleep(1)
        save_window = wait_for_window("Speichern unter", timeout=10)
        if save_window is None:
            print("Das Fenster 'Speichern unter' wurde nicht gefunden!")
            return
        
        pyautogui.write(f"{filename}-{i}", interval=0.05)
        pyautogui.press('enter')
        time.sleep(5)
        print("Warte, bis das Programm wieder reagiert...")
        if wait_until_program_responsive(hwnd, timeout=300):
            print("Programm reagiert wieder.")
            if SOUND:
              winsound.Beep(frequency, duration)
        else:
            print("Programm reagiert auch nach 5min nicht.")
            return
        time.sleep(1)  # Kurze Pause vor dem nächsten Export
        pyautogui.hotkey( 'alt', 'f')
        time.sleep(0.1)
        pyautogui.press('e')
        time.sleep(0.1)
        pyautogui.press('a')
        time.sleep(0.1)
        for _ in range((i*2)):
            pyautogui.press('down')
            time.sleep(1)
        time.sleep(5)
        pyautogui.press('enter')
        
        # Einmal Pfeil nach unten und Enter drücken
        # pyautogui.press('down')
        # pyautogui.press('enter')
        
        # Dateiname mit Endung "-<TraceNr>" eingeben und mit Enter bestätigen
        #time.sleep(1000)
        save_window = wait_for_window("Speichern unter", timeout=10)
        if save_window is None:
            print("Das Fenster 'Speichern unter' wurde nicht gefunden!")
            return
        
        pyautogui.write(f"{filename}-{i}D", interval=0.05)
        pyautogui.press('enter')
        time.sleep(5)  # Kurze Pause vor dem nächsten Export
        if wait_until_program_responsive(hwnd, timeout=300):
            print("Programm reagiert wieder.")
            winsound.Beep(frequency, duration)
        else:
            print("Programm reagiert auch nach 5min nicht.")
            return
        time.sleep(1)

if __name__ == '__main__':
    main()
