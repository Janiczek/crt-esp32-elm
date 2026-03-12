const int buttonPin = 0;
const int ledPin = 2;

int counter = 0;
bool buttonPressed = false;

void setup()
{
  pinMode(buttonPin, INPUT_PULLUP);
  pinMode(ledPin, OUTPUT);
}

void loop()
{
  bool newButtonPressed = !digitalRead(buttonPin);
  digitalWrite(ledPin,newButtonPressed); // led ON when button pressed
  if (newButtonPressed && !buttonPressed) {
    counter++;
  }
  buttonPressed = newButtonPressed;
}
