// Arduino NANO 33 IoT

int pulsePin = 9;
int ledPin = 12; // Internal LED 
int buttonPin = 7; // Pin for button

int pulseDurationA = 100; // milliseconds
int pulseDurationB = 30000; // milliseconds
int pulseDurationC = 40000; // milliseconds
int pulseDurationD = 20000; // milliseconds


bool pulseActive = false;

void setup() {
  pinMode(pulsePin, OUTPUT);
  pinMode(ledPin, OUTPUT); // Set internal LED pin as output
  pinMode(buttonPin, INPUT_PULLUP); // Set button pin as input with internal pull-up resistor
  Serial.begin(9600);
  delay(1);
  Serial.println("Press the button to start the pulse sequence.");
  Serial.println("Enter new pulse durations in milliseconds (e.g., A=50ms B=30ms C=1500ms):");
  Serial.println("Format: A=<value> B=<value> C=<value>");
}

void loop() {
  // Check for button press
  if (digitalRead(buttonPin) == LOW) {
    pulseActive = true;
    Serial.println("Button pressed. Starting pulse sequence.");
  }

  // Check for serial input
  if (Serial.available() > 0) {
    String input = Serial.readStringUntil('\n');
    parseInput(input);
  }

  // Run pulse sequence if activated
  if (pulseActive) {
    runPulseSequence();
  }
}

void runPulseSequence() {
  // Pulse sequence for 1 shortPulse
  digitalWrite(ledPin, HIGH);
  delay(pulseDurationA);
  digitalWrite(ledPin, LOW);
  delay(pulseDurationA);
  digitalWrite(ledPin, HIGH);
  delay(pulseDurationA);
  digitalWrite(ledPin, LOW);
  delay(pulseDurationA);
  digitalWrite(ledPin, HIGH);
  delay(pulseDurationA);
  digitalWrite(ledPin, LOW);
  delay(pulseDurationD);
  digitalWrite(pulsePin, HIGH);
  digitalWrite(ledPin, HIGH);
  delay(pulseDurationA);
  digitalWrite(pulsePin, LOW);
  digitalWrite(ledPin, LOW);
  delay(pulseDurationB);

  // Pulse sequence for 2 shortPulse
  digitalWrite(pulsePin, HIGH);
  digitalWrite(ledPin, HIGH);
  delay(pulseDurationA);
  digitalWrite(pulsePin, LOW);
  digitalWrite(ledPin, LOW);
  delay(pulseDurationB);

    // Pulse sequence for 3 shortPulse
  digitalWrite(pulsePin, HIGH);
  digitalWrite(ledPin, HIGH);
  delay(pulseDurationA);
  digitalWrite(pulsePin, LOW);
  digitalWrite(ledPin, LOW);
  delay(pulseDurationB);

    // Pulse sequence for 4 shortPulse
  digitalWrite(pulsePin, HIGH);
  digitalWrite(ledPin, HIGH);
  delay(pulseDurationA);
  digitalWrite(pulsePin, LOW);
  digitalWrite(ledPin, LOW);
  delay(pulseDurationB);


  // Pulse sequence for 5 shortPulse
  digitalWrite(pulsePin, HIGH);
  digitalWrite(ledPin, HIGH);
  delay(pulseDurationA);
  digitalWrite(pulsePin, LOW);
  digitalWrite(ledPin, LOW);
  delay(pulseDurationC);


}

void parseInput(String input) {
  int newPulseDurationA = pulseDurationA;
  int newPulseDurationB = pulseDurationB;
  int newPulseDurationC = pulseDurationC;
  int newPulseDurationD = pulseDurationD;

  // Parse the input
  if (input.startsWith("A=")) {
    int index = input.indexOf(' ');
    newPulseDurationA = input.substring(2, index).toInt();
    input = input.substring(index + 1);
  }
  if (input.startsWith("B=")) {
    int index = input.indexOf(' ');
    newPulseDurationB = input.substring(2, index).toInt();
    input = input.substring(index + 1);
  }
  if (input.startsWith("C=")) {
    newPulseDurationC = input.substring(2).toInt();
  }

  // Update pulse durations
  pulseDurationA = newPulseDurationA;
  pulseDurationB = newPulseDurationB;
  pulseDurationC = newPulseDurationC;
  pulseDurationD = newPulseDurationD;

  // Confirm the new settings
  Serial.print("New pulse durations set: A=");
  Serial.print(pulseDurationA);
  Serial.print(" B=");
  Serial.print(pulseDurationB);
  Serial.print(" C=");
  Serial.println(pulseDurationC);
  Serial.print(" D=");
  Serial.println(pulseDurationD);
}
