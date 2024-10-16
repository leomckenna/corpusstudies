#!/Users/danielshanahan/opt/anaconda3/bin/python3

from music21 import converter, pitch
score = converter.parse('happy_birthday.krn')

# Extract all pitches
all_pitches = score.pitches

# Get unique pitches
unique_pitches = set(all_pitches)

# Count unique pitches
unique_pitch_count = len(unique_pitches)

# Print results
print(f"Number of unique pitches: {unique_pitch_count}")
print("\nUnique pitches:")
for p in sorted(unique_pitches):
    print(str(p))
