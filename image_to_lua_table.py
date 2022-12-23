# Code generated with Chat GPT for fun

# Intial prompt:

# Create a python script that does the following:
# 1. scan a folder `input_dir` for images
# 2. if the output folder `output_dir` does not contain a file with the same name, but ending in `.lua` then
#  2a. read the image
#  2b. output a file as defined in step 3 based on the read image
# 3. output to file a lua table as text formated like this { width={image width}, height={image height}, chunks={chunks}
# where `chunks` is another table with base64 encoded text. the base64 text is from reading pixel data and using struct.pack from python in big endian and bytes in the order ARGB
#
# If anything is unclear ask questions.

# Additional prompts used to revise code + some edits done myself
# Overall it did pretty well, but didn't get the whole way there on it's own

import os
import struct
import base64

import PIL.Image


def read_image(image_path):
    # Read the image
    image = PIL.Image.open(image_path)
    image = image.convert("RGBA") # TheIncgi
    # Get the image width and height
    width, height = image.size

    # Initialize a list to store the pixel data
    # pixel_data = []

    pixel_data = []

    # Loop through the pixels in the image
    for y in range(height):
        for x in range(width):
            # Get the pixel at (x, y)
            # pixel = image.getpixel((x, y))
            r,g,b,a = image.getpixel((x, y))

            # Append the pixel data to the list in big-endian ARGB order
            # pixel_data.append(struct.pack('>I', pixel))
            pixel_data.append(struct.pack("BBBB", a,r,g,b) ) #TheIncgi

    pixel_data_b64 = []
    # Encode the pixel data as a list of base64 strings, with at most 128 pixels per string
    for i in range(0, len(pixel_data), 128):
        pixel_data_bytes = bytes.join(b"",pixel_data[i:i+128])
        # pixel_data_str = pixel_data_bytes.encode()
        pixel_data_b64.append(base64.b64encode(pixel_data_bytes))



    # Return the image width, height, and pixel data as a tuple
    return width, height, pixel_data_b64


def write_output_file(output_path, width, height, pixel_data_b64):
    # Open the output file for writing
    with open(output_path, "w") as f:
        # Write the Lua table to the file
        f.write("return {\n")
        f.write("    width = {},\n".format(width))
        f.write("    height = {},\n".format(height))
        f.write("    chunks = {\n")
        for chunk in pixel_data_b64:
            f.write("        [[{}]],\n".format(chunk.decode()))
        f.write("    }\n")
        f.write("}\n")



def main():
    # Set the input and output directories
    input_dir = 'textures'
    output_dir = 'output/textures'

    # Loop through the files in the input directory
    for filename in os.listdir(input_dir):
        # Check if the file is an image
        if filename.endswith('.png'):
            # Construct the input and output file paths
            input_path = os.path.join(input_dir, filename)
            output_path = os.path.join(output_dir, filename[:-4] + '.lua')

            # Check if the output file already exists
            if not os.path.exists(output_path):
                # Read the image and get the image width, height, and pixel data
                width, height, pixel_data_b64 = read_image(input_path)

                # Write the output file
                write_output_file(output_path, width, height, pixel_data_b64)


if __name__ == '__main__':
    main()
