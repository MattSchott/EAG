# written by matthias.schott@uni-bayreuth.de
# Creative Commons  BY-NC-SA 4.0
# Attribution-NonCommercial-ShareAlike


import cv2
import numpy as np
import pyautogui
import keyboard
import tkinter as tk
from PIL import Image, ImageTk

root = None  # Global reference to the overlay window

def take_screenshot():
    """Capture a screenshot and return it as a PIL Image."""
    return pyautogui.screenshot()

def show_transparent_screenshot():
    """Display the screenshot in a frameless, always-on-top, semi-transparent Tkinter window."""
    global root
    if root and root.winfo_exists():
        root.destroy()  # Close existing window before creating a new one

    screenshot = take_screenshot().convert("RGBA")

    # Apply transparency (50%)
    #alpha = 128
    alpha = 0
    overlay = Image.new("RGBA", screenshot.size, (0, 0, 0, 0))
    transparent_screenshot = Image.blend(screenshot, overlay, alpha / 255)

    # Create a Tkinter window
    root = tk.Toplevel()
    root.attributes("-fullscreen", True)
    root.attributes("-alpha", 0.5)
    root.overrideredirect(True)

    # Convert image for Tkinter
    img_tk = ImageTk.PhotoImage(transparent_screenshot)

    # Create a label to display the image
    label = tk.Label(root, image=img_tk)
    label.image = img_tk
    label.pack()

    # Allow toggling but prevent unintended closing
    root.bind("<Button-1>", lambda e: None)  # Disable closing on click
    root.bind("<KeyPress-F2>", lambda e: toggle_window_state())  # Only close with F2

    root.mainloop()

def toggle_window_state():
    """Toggle overlay visibility, allowing it to be minimized and restored."""
    global root
    if root and root.winfo_exists():
        if root.state() == "iconic":  # If minimized, restore
            root.deiconify()
            root.lift()
            root.focus_force()
        else:
            root.iconify()  # Minimize window

def main():
    print("Press F1 to take a screenshot with transparency effect...")
    keyboard.add_hotkey("F1", show_transparent_screenshot)  # Capture screenshot on F1
    keyboard.add_hotkey("F2", toggle_window_state)  # Toggle window state on F2
    keyboard.wait()  # Keep script running

if __name__ == "__main__":
    main()
