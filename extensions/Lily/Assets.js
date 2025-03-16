// Name: Asset Manager
// ID: lmsAssets
// Description: Add, remove, and get data from various types of assets.
// License: MIT AND LGPL-3.0

// TheShovel is so epic and cool and awesome

(function (Scratch) {
  "use strict";

  const vm = Scratch.vm;
  const runtime = vm.runtime;
  const Cast = Scratch.Cast;

  const requireNonPackagedRuntime = (blockName) => {
    if (vm.runtime.isPackaged) {
      alert(
        `To use the Asset Manager ${blockName} block, the creator of the packaged project must uncheck "Remove raw asset data after loading to save RAM" under advanced settings in the packager.`
      );
      return false;
    }
    return true;
  };
  
  // code from https://github.com/deanm/omggif/blob/master/omggif.js
  
  // (c) Dean McNamee <dean@gmail.com>, 2013.
//
// https://github.com/deanm/omggif
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
//
// omggif is a JavaScript implementation of a GIF 89a encoder and decoder,
// including animation and compression.  It does not rely on any specific
// underlying system, so should run in the browser, Node, or Plask.
  
  function GifReader(buf) {
  var p = 0;

  // - Header (GIF87a or GIF89a).
  if (buf[p++] !== 0x47 ||            buf[p++] !== 0x49 || buf[p++] !== 0x46 ||
      buf[p++] !== 0x38 || (buf[p++]+1 & 0xfd) !== 0x38 || buf[p++] !== 0x61) {
    throw new Error("Invalid GIF 87a/89a header.");
  }

  // - Logical Screen Descriptor.
  var width = buf[p++] | buf[p++] << 8;
  var height = buf[p++] | buf[p++] << 8;
  var pf0 = buf[p++];  // <Packed Fields>.
  var global_palette_flag = pf0 >> 7;
  var num_global_colors_pow2 = pf0 & 0x7;
  var num_global_colors = 1 << (num_global_colors_pow2 + 1);
  var background = buf[p++];
  buf[p++];  // Pixel aspect ratio (unused?).

  var global_palette_offset = null;
  var global_palette_size   = null;

  if (global_palette_flag) {
    global_palette_offset = p;
    global_palette_size = num_global_colors;
    p += num_global_colors * 3;  // Seek past palette.
  }

  var no_eof = true;

  var frames = [ ];

  var delay = 0;
  var transparent_index = null;
  var disposal = 0;  // 0 - No disposal specified.
  var loop_count = null;

  this.width = width;
  this.height = height;

  while (no_eof && p < buf.length) {
    switch (buf[p++]) {
      case 0x21:  // Graphics Control Extension Block
        switch (buf[p++]) {
          case 0xff:  // Application specific block
            // Try if it's a Netscape block (with animation loop counter).
            if (buf[p   ] !== 0x0b ||  // 21 FF already read, check block size.
                // NETSCAPE2.0
                buf[p+1 ] == 0x4e && buf[p+2 ] == 0x45 && buf[p+3 ] == 0x54 &&
                buf[p+4 ] == 0x53 && buf[p+5 ] == 0x43 && buf[p+6 ] == 0x41 &&
                buf[p+7 ] == 0x50 && buf[p+8 ] == 0x45 && buf[p+9 ] == 0x32 &&
                buf[p+10] == 0x2e && buf[p+11] == 0x30 &&
                // Sub-block
                buf[p+12] == 0x03 && buf[p+13] == 0x01 && buf[p+16] == 0) {
              p += 14;
              loop_count = buf[p++] | buf[p++] << 8;
              p++;  // Skip terminator.
            } else {  // We don't know what it is, just try to get past it.
              p += 12;
              while (true) {  // Seek through subblocks.
                var block_size = buf[p++];
                // Bad block size (ex: undefined from an out of bounds read).
                if (!(block_size >= 0)) throw Error("Invalid block size");
                if (block_size === 0) break;  // 0 size is terminator
                p += block_size;
              }
            }
            break;

          case 0xf9:  // Graphics Control Extension
            if (buf[p++] !== 0x4 || buf[p+4] !== 0)
              throw new Error("Invalid graphics extension block.");
            var pf1 = buf[p++];
            delay = buf[p++] | buf[p++] << 8;
            transparent_index = buf[p++];
            if ((pf1 & 1) === 0) transparent_index = null;
            disposal = pf1 >> 2 & 0x7;
            p++;  // Skip terminator.
            break;

          // Plain Text Extension could be present and we just want to be able
          // to parse past it.  It follows the block structure of the comment
          // extension enough to reuse the path to skip through the blocks.
          case 0x01:  // Plain Text Extension (fallthrough to Comment Extension)
          case 0xfe:  // Comment Extension.
            while (true) {  // Seek through subblocks.
              var block_size = buf[p++];
              // Bad block size (ex: undefined from an out of bounds read).
              if (!(block_size >= 0)) throw Error("Invalid block size");
              if (block_size === 0) break;  // 0 size is terminator
              // console.log(buf.slice(p, p+block_size).toString('ascii'));
              p += block_size;
            }
            break;

          default:
            throw new Error(
                "Unknown graphic control label: 0x" + buf[p-1].toString(16));
        }
        break;

      case 0x2c:  // Image Descriptor.
        var x = buf[p++] | buf[p++] << 8;
        var y = buf[p++] | buf[p++] << 8;
        var w = buf[p++] | buf[p++] << 8;
        var h = buf[p++] | buf[p++] << 8;
        var pf2 = buf[p++];
        var local_palette_flag = pf2 >> 7;
        var interlace_flag = pf2 >> 6 & 1;
        var num_local_colors_pow2 = pf2 & 0x7;
        var num_local_colors = 1 << (num_local_colors_pow2 + 1);
        var palette_offset = global_palette_offset;
        var palette_size = global_palette_size;
        var has_local_palette = false;
        if (local_palette_flag) {
          var has_local_palette = true;
          palette_offset = p;  // Override with local palette.
          palette_size = num_local_colors;
          p += num_local_colors * 3;  // Seek past palette.
        }

        var data_offset = p;

        p++;  // codesize
        while (true) {
          var block_size = buf[p++];
          // Bad block size (ex: undefined from an out of bounds read).
          if (!(block_size >= 0)) throw Error("Invalid block size");
          if (block_size === 0) break;  // 0 size is terminator
          p += block_size;
        }

        frames.push({x: x, y: y, width: w, height: h,
                     has_local_palette: has_local_palette,
                     palette_offset: palette_offset,
                     palette_size: palette_size,
                     data_offset: data_offset,
                     data_length: p - data_offset,
                     transparent_index: transparent_index,
                     interlaced: !!interlace_flag,
                     delay: delay,
                     disposal: disposal});
        break;

      case 0x3b:  // Trailer Marker (end of file).
        no_eof = false;
        break;

      default:
        throw new Error("Unknown gif block: 0x" + buf[p-1].toString(16));
        break;
    }
  }

  this.numFrames = function() {
    return frames.length;
  };

  this.loopCount = function() {
    return loop_count;
  };

  this.frameInfo = function(frame_num) {
    if (frame_num < 0 || frame_num >= frames.length)
      throw new Error("Frame index out of range.");
    return frames[frame_num];
  };

  this.decodeAndBlitFrameBGRA = function(frame_num, pixels) {
    var frame = this.frameInfo(frame_num);
    var num_pixels = frame.width * frame.height;
    var index_stream = new Uint8Array(num_pixels);  // At most 8-bit indices.
    GifReaderLZWOutputIndexStream(
        buf, frame.data_offset, index_stream, num_pixels);
    var palette_offset = frame.palette_offset;

    // NOTE(deanm): It seems to be much faster to compare index to 256 than
    // to === null.  Not sure why, but CompareStub_EQ_STRICT shows up high in
    // the profile, not sure if it's related to using a Uint8Array.
    var trans = frame.transparent_index;
    if (trans === null) trans = 256;

    // We are possibly just blitting to a portion of the entire frame.
    // That is a subrect within the framerect, so the additional pixels
    // must be skipped over after we finished a scanline.
    var framewidth  = frame.width;
    var framestride = width - framewidth;
    var xleft       = framewidth;  // Number of subrect pixels left in scanline.

    // Output index of the top left corner of the subrect.
    var opbeg = ((frame.y * width) + frame.x) * 4;
    // Output index of what would be the left edge of the subrect, one row
    // below it, i.e. the index at which an interlace pass should wrap.
    var opend = ((frame.y + frame.height) * width + frame.x) * 4;
    var op    = opbeg;

    var scanstride = framestride * 4;

    // Use scanstride to skip past the rows when interlacing.  This is skipping
    // 7 rows for the first two passes, then 3 then 1.
    if (frame.interlaced === true) {
      scanstride += width * 4 * 7;  // Pass 1.
    }

    var interlaceskip = 8;  // Tracking the row interval in the current pass.

    for (var i = 0, il = index_stream.length; i < il; ++i) {
      var index = index_stream[i];

      if (xleft === 0) {  // Beginning of new scan line
        op += scanstride;
        xleft = framewidth;
        if (op >= opend) { // Catch the wrap to switch passes when interlacing.
          scanstride = framestride * 4 + width * 4 * (interlaceskip-1);
          // interlaceskip / 2 * 4 is interlaceskip << 1.
          op = opbeg + (framewidth + framestride) * (interlaceskip << 1);
          interlaceskip >>= 1;
        }
      }

      if (index === trans) {
        op += 4;
      } else {
        var r = buf[palette_offset + index * 3];
        var g = buf[palette_offset + index * 3 + 1];
        var b = buf[palette_offset + index * 3 + 2];
        pixels[op++] = b;
        pixels[op++] = g;
        pixels[op++] = r;
        pixels[op++] = 255;
      }
      --xleft;
    }
  };

  // I will go to copy and paste hell one day...
  this.decodeAndBlitFrameRGBA = function(frame_num, pixels) {
    var frame = this.frameInfo(frame_num);
    var num_pixels = frame.width * frame.height;
    var index_stream = new Uint8Array(num_pixels);  // At most 8-bit indices.
    GifReaderLZWOutputIndexStream(
        buf, frame.data_offset, index_stream, num_pixels);
    var palette_offset = frame.palette_offset;

    // NOTE(deanm): It seems to be much faster to compare index to 256 than
    // to === null.  Not sure why, but CompareStub_EQ_STRICT shows up high in
    // the profile, not sure if it's related to using a Uint8Array.
    var trans = frame.transparent_index;
    if (trans === null) trans = 256;

    // We are possibly just blitting to a portion of the entire frame.
    // That is a subrect within the framerect, so the additional pixels
    // must be skipped over after we finished a scanline.
    var framewidth  = frame.width;
    var framestride = width - framewidth;
    var xleft       = framewidth;  // Number of subrect pixels left in scanline.

    // Output index of the top left corner of the subrect.
    var opbeg = ((frame.y * width) + frame.x) * 4;
    // Output index of what would be the left edge of the subrect, one row
    // below it, i.e. the index at which an interlace pass should wrap.
    var opend = ((frame.y + frame.height) * width + frame.x) * 4;
    var op    = opbeg;

    var scanstride = framestride * 4;

    // Use scanstride to skip past the rows when interlacing.  This is skipping
    // 7 rows for the first two passes, then 3 then 1.
    if (frame.interlaced === true) {
      scanstride += width * 4 * 7;  // Pass 1.
    }

    var interlaceskip = 8;  // Tracking the row interval in the current pass.

    for (var i = 0, il = index_stream.length; i < il; ++i) {
      var index = index_stream[i];

      if (xleft === 0) {  // Beginning of new scan line
        op += scanstride;
        xleft = framewidth;
        if (op >= opend) { // Catch the wrap to switch passes when interlacing.
          scanstride = framestride * 4 + width * 4 * (interlaceskip-1);
          // interlaceskip / 2 * 4 is interlaceskip << 1.
          op = opbeg + (framewidth + framestride) * (interlaceskip << 1);
          interlaceskip >>= 1;
        }
      }

      if (index === trans) {
        op += 4;
      } else {
        var r = buf[palette_offset + index * 3];
        var g = buf[palette_offset + index * 3 + 1];
        var b = buf[palette_offset + index * 3 + 2];
        pixels[op++] = r;
        pixels[op++] = g;
        pixels[op++] = b;
        pixels[op++] = 255;
      }
      --xleft;
    }
  };
}

function GifReaderLZWOutputIndexStream(code_stream, p, output, output_length) {
  var min_code_size = code_stream[p++];

  var clear_code = 1 << min_code_size;
  var eoi_code = clear_code + 1;
  var next_code = eoi_code + 1;

  var cur_code_size = min_code_size + 1;  // Number of bits per code.
  // NOTE: This shares the same name as the encoder, but has a different
  // meaning here.  Here this masks each code coming from the code stream.
  var code_mask = (1 << cur_code_size) - 1;
  var cur_shift = 0;
  var cur = 0;

  var op = 0;  // Output pointer.

  var subblock_size = code_stream[p++];

  // TODO(deanm): Would using a TypedArray be any faster?  At least it would
  // solve the fast mode / backing store uncertainty.
  // var code_table = Array(4096);
  var code_table = new Int32Array(4096);  // Can be signed, we only use 20 bits.

  var prev_code = null;  // Track code-1.

  while (true) {
    // Read up to two bytes, making sure we always 12-bits for max sized code.
    while (cur_shift < 16) {
      if (subblock_size === 0) break;  // No more data to be read.

      cur |= code_stream[p++] << cur_shift;
      cur_shift += 8;

      if (subblock_size === 1) {  // Never let it get to 0 to hold logic above.
        subblock_size = code_stream[p++];  // Next subblock.
      } else {
        --subblock_size;
      }
    }

    // TODO(deanm): We should never really get here, we should have received
    // and EOI.
    if (cur_shift < cur_code_size)
      break;

    var code = cur & code_mask;
    cur >>= cur_code_size;
    cur_shift -= cur_code_size;

    // TODO(deanm): Maybe should check that the first code was a clear code,
    // at least this is what you're supposed to do.  But actually our encoder
    // now doesn't emit a clear code first anyway.
    if (code === clear_code) {
      // We don't actually have to clear the table.  This could be a good idea
      // for greater error checking, but we don't really do any anyway.  We
      // will just track it with next_code and overwrite old entries.

      next_code = eoi_code + 1;
      cur_code_size = min_code_size + 1;
      code_mask = (1 << cur_code_size) - 1;

      // Don't update prev_code ?
      prev_code = null;
      continue;
    } else if (code === eoi_code) {
      break;
    }

    // We have a similar situation as the decoder, where we want to store
    // variable length entries (code table entries), but we want to do in a
    // faster manner than an array of arrays.  The code below stores sort of a
    // linked list within the code table, and then "chases" through it to
    // construct the dictionary entries.  When a new entry is created, just the
    // last byte is stored, and the rest (prefix) of the entry is only
    // referenced by its table entry.  Then the code chases through the
    // prefixes until it reaches a single byte code.  We have to chase twice,
    // first to compute the length, and then to actually copy the data to the
    // output (backwards, since we know the length).  The alternative would be
    // storing something in an intermediate stack, but that doesn't make any
    // more sense.  I implemented an approach where it also stored the length
    // in the code table, although it's a bit tricky because you run out of
    // bits (12 + 12 + 8), but I didn't measure much improvements (the table
    // entries are generally not the long).  Even when I created benchmarks for
    // very long table entries the complexity did not seem worth it.
    // The code table stores the prefix entry in 12 bits and then the suffix
    // byte in 8 bits, so each entry is 20 bits.

    var chase_code = code < next_code ? code : prev_code;

    // Chase what we will output, either {CODE} or {CODE-1}.
    var chase_length = 0;
    var chase = chase_code;
    while (chase > clear_code) {
      chase = code_table[chase] >> 8;
      ++chase_length;
    }

    var k = chase;

    var op_end = op + chase_length + (chase_code !== code ? 1 : 0);
    if (op_end > output_length) {
      console.log("Warning, gif stream longer than expected.");
      return;
    }

    // Already have the first byte from the chase, might as well write it fast.
    output[op++] = k;

    op += chase_length;
    var b = op;  // Track pointer, writing backwards.

    if (chase_code !== code)  // The case of emitting {CODE-1} + k.
      output[op++] = k;

    chase = chase_code;
    while (chase_length--) {
      chase = code_table[chase];
      output[--b] = chase & 0xff;  // Write backwards.
      chase >>= 8;  // Pull down to the prefix code.
    }

    if (prev_code !== null && next_code < 4096) {
      code_table[next_code++] = prev_code << 8 | k;
      // TODO(deanm): Figure out this clearing vs code growth logic better.  I
      // have an feeling that it should just happen somewhere else, for now it
      // is awkward between when we grow past the max and then hit a clear code.
      // For now just check if we hit the max 12-bits (then a clear code should
      // follow, also of course encoded in 12-bits).
      if (next_code >= code_mask+1 && cur_code_size < 12) {
        ++cur_code_size;
        code_mask = code_mask << 1 | 1;
      }
    }

    prev_code = code;
  }

  if (op !== output_length) {
    console.log("Warning, gif stream shorter than expected.");
  }

  return output;
}

// End of code from https://github.com/deanm/omggif/blob/master/omggif.js

// https://github.com/TurboWarp/scratch-gui/blob/develop/src/lib/gif-decoder.js
const gifDecoder = (arrayBuffer, onFrame) => {
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    const gifReader = new GifReader(new Uint8Array(arrayBuffer));
    const numFrames = gifReader.numFrames();
    canvas.width = gifReader.width;
    canvas.height = gifReader.height;

    let imageData = ctx.createImageData(canvas.width, canvas.height);
    let previousData = ctx.createImageData(canvas.width, canvas.height);

    const loadFrame = i => {
        const framePixels = [];
        gifReader.decodeAndBlitFrameRGBA(i, framePixels);
        const {x, y, width, height, disposal} = gifReader.frameInfo(i);
        for (let row = 0; row < height; row++) {
            for (let column = 0; column < width; column++) {
                const indexOffset = 4 * (x + (y * canvas.width));
                const j = indexOffset + (4 * (column + (row * canvas.width)));
                if (framePixels[j + 3]) {
                    imageData.data[j + 0] = framePixels[j + 0];
                    imageData.data[j + 1] = framePixels[j + 1];
                    imageData.data[j + 2] = framePixels[j + 2];
                    imageData.data[j + 3] = framePixels[j + 3];
                }
            }
        }

        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.putImageData(imageData, 0, 0);

        const dataUrl = canvas.toDataURL();

        switch (disposal) {
        case 2: // "Return to background", blank out the current frame
            ctx.clearRect(x, y, width, height);
            imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
            break;
        case 3: // "Restore to previous", copy previous data to current
            imageData = ctx.createImageData(canvas.width, canvas.height);
            imageData.data.set(previousData.data);
            break;
        default: // 0 and 1, as well as 4+ modes = do-not-dispose, so cache frame
            previousData = ctx.getImageData(0, 0, canvas.width, canvas.height);
            break;

        }
        onFrame(i, dataUrl, numFrames);

        if (i < numFrames - 1) {
            setTimeout(() => {
                loadFrame(i + 1);
            });
        }
    };

    loadFrame(0);
};

  class Assets {
    getInfo() {
      return {
        id: "lmsAssets",
        color1: "#5779ca",
        color2: "#4e6db6",
        color3: "#4661a2",
        name: Scratch.translate("Asset Manager"),
        blocks: [
          {
            opcode: "addSprite",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("add sprite from URL [URL]"),
            arguments: {
              URL: {
                type: Scratch.ArgumentType.STRING,
              },
            },
          },
          {
            opcode: "addCostume",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("add costume from URL [URL] named [NAME]"),
            arguments: {
              URL: {
                type: Scratch.ArgumentType.STRING,
              },
              NAME: {
                type: Scratch.ArgumentType.STRING,
                defaultValue: "costume1",
              },
            },
          },
          {
            opcode: "addSound",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("add sound from URL [URL] named [NAME]"),
            arguments: {
              URL: {
                type: Scratch.ArgumentType.STRING,
              },
              NAME: {
                type: Scratch.ArgumentType.STRING,
                defaultValue: "sound1",
              },
            },
          },
          "---",
          {
            opcode: "renameSprite",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("rename sprite [TARGET] to [NAME]"),
            arguments: {
              TARGET: {
                type: Scratch.ArgumentType.STRING,
                menu: "targets",
              },
              NAME: {
                type: Scratch.ArgumentType.STRING,
                defaultValue: "Sprite1",
              },
            },
          },
          {
            opcode: "renameCostume",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("rename costume [COSTUME] to [NAME]"),
            arguments: {
              COSTUME: {
                type: Scratch.ArgumentType.COSTUME,
              },
              NAME: {
                type: Scratch.ArgumentType.STRING,
                defaultValue: "costume1",
              },
            },
          },
          {
            opcode: "renameSound",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("rename sound [SOUND] to [NAME]"),
            arguments: {
              SOUND: {
                type: Scratch.ArgumentType.SOUND,
              },
              NAME: {
                type: Scratch.ArgumentType.STRING,
                defaultValue: "sound1",
              },
            },
          },
          "---",
          {
            opcode: "deleteSprite",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("delete sprite [TARGET]"),
            arguments: {
              TARGET: {
                type: Scratch.ArgumentType.STRING,
                menu: "targets",
              },
              NAME: {
                type: Scratch.ArgumentType.STRING,
                defaultValue: "Sprite1",
              },
            },
          },
          {
            opcode: "deleteCostume",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("delete costume [COSTUME]"),
            arguments: {
              COSTUME: {
                type: Scratch.ArgumentType.COSTUME,
              },
              NAME: {
                type: Scratch.ArgumentType.STRING,
                defaultValue: "costume1",
              },
            },
          },
          {
            opcode: "deleteSound",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("delete sound [SOUND]"),
            arguments: {
              SOUND: {
                type: Scratch.ArgumentType.SOUND,
              },
              NAME: {
                type: Scratch.ArgumentType.STRING,
                defaultValue: "sound1",
              },
            },
          },
          "---",
          {
            opcode: "getAllSprites",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("all sprites"),
          },
          {
            opcode: "getAllCostumes",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("all costumes"),
          },
          {
            opcode: "getAllSounds",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("all sounds"),
          },
          {
            opcode: "getSpriteName",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("sprite name"),
          },
          "---",
          {
            opcode: "reorderCostume",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate(
              "reorder costume # [INDEX1] to index [INDEX2]"
            ),
            arguments: {
              INDEX1: {
                type: Scratch.ArgumentType.NUMBER,
                defaultValue: "1",
              },
              INDEX2: {
                type: Scratch.ArgumentType.NUMBER,
                defaultValue: "2",
              },
            },
          },
          {
            opcode: "reorderSound",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate(
              "reorder sound # [INDEX1] to index [INDEX2]"
            ),
            arguments: {
              INDEX1: {
                type: Scratch.ArgumentType.NUMBER,
                defaultValue: "1",
              },
              INDEX2: {
                type: Scratch.ArgumentType.NUMBER,
                defaultValue: "2",
              },
            },
          },
          "---",
          {
            opcode: "getSoundData",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("[ATTRIBUTE] of [SOUND]"),
            arguments: {
              ATTRIBUTE: {
                type: Scratch.ArgumentType.STRING,
                menu: "attribute",
              },
              SOUND: {
                type: Scratch.ArgumentType.SOUND,
              },
            },
          },
          {
            opcode: "getCostumeData",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("[ATTRIBUTE] of [COSTUME]"),
            arguments: {
              ATTRIBUTE: {
                type: Scratch.ArgumentType.STRING,
                menu: "attribute",
              },
              COSTUME: {
                type: Scratch.ArgumentType.COSTUME,
              },
            },
          },
          "---",
          {
            opcode: "getCostumeAtIndex",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("name of costume # [INDEX]"),
            arguments: {
              INDEX: {
                type: Scratch.ArgumentType.NUMBER,
                defaultValue: "1",
              },
            },
          },
          {
            opcode: "getSoundAtIndex",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("name of sound # [INDEX]"),
            arguments: {
              INDEX: {
                type: Scratch.ArgumentType.NUMBER,
                defaultValue: "1",
              },
            },
          },
          "---",
          {
            opcode: "openProject",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("open project from URL [URL]"),
            arguments: {
              URL: {
                type: Scratch.ArgumentType.STRING,
              },
            },
          },
          {
            opcode: "getProjectJSON",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("project JSON"),
          },
          "---",
          {
            opcode: "loadExtension",
            blockType: Scratch.BlockType.COMMAND,
            text: Scratch.translate("load extension from URL [URL]"),
            arguments: {
              URL: {
                type: Scratch.ArgumentType.STRING,
                defaultValue:
                  "https://extensions.turbowarp.org/Skyhigh173/json.js",
              },
            },
          },
          {
            opcode: "getLoadedExtensions",
            blockType: Scratch.BlockType.REPORTER,
            text: Scratch.translate("loaded extensions"),
          },
        ],
        menus: {
          targets: {
            acceptReporters: true,
            items: "_getTargets",
          },
          attribute: {
            acceptReporters: false,
            items: [
              {
                text: Scratch.translate("index"),
                value: "index",
              },
              {
                text: Scratch.translate("dataURI"),
                value: "dataURI",
              },
              {
                text: Scratch.translate("format"),
                value: "format",
              },
              {
                text: Scratch.translate("header"),
                value: "header",
              },
              {
                text: Scratch.translate("asset ID"),
                value: "asset ID",
              },
            ],
          },
        },
      };
    }

    async addSprite(args, util) {
      const url = Cast.toString(args.URL);

      const response = await Scratch.fetch(url);
      const json = await response.arrayBuffer();

      try {
        await vm.addSprite(json);
      } catch (e) {
        console.error(e);
      }
    }

    // Thank you PenguinMod for providing this code.
    async addCostume(args, util) {
      const targetId = util.target.id;
      const assetName = Cast.toString(args.NAME);

      const res = await Scratch.fetch(args.URL);
      const blob = await res.blob();

      if (!(this._typeIsBitmap(blob.type) || blob.type === "image/svg+xml")) {
        console.error(`Invalid MIME type: ${blob.type}`);
        return;
      }

      if (blob.type == "image/gif") {
        const arrayBuffer = await blob.arrayBuffer();
        
        gifDecoder(arrayBuffer, (frameNumber, dataUrl, numFrames) => {
            this._handleGif(frameNumber, dataUrl, numFrames, assetName, targetId)
        });
      } else {
        const assetType = this._typeIsBitmap(blob.type)
        ? runtime.storage.AssetType.ImageBitmap
        : runtime.storage.AssetType.ImageVector;

        // Bitmap data format is not actually enforced, but setting it to something that isn't in scratch-parser's
        // known format list will throw an error when someone tries to load the project.
        // (https://github.com/scratchfoundation/scratch-parser/blob/665f05d739a202d565a4af70a201909393d456b2/lib/sb3_definitions.json#L51)
        const dataType =
          blob.type === "image/svg+xml"
            ? runtime.storage.DataFormat.SVG
            : runtime.storage.DataFormat.PNG;
          
        const arrayBuffer = await new Promise((resolve, reject) => {
          const fr = new FileReader();
          fr.onload = () => resolve(fr.result);
          fr.onerror = () =>
            reject(new Error(`Failed to read as array buffer: ${fr.error}`));
          fr.readAsArrayBuffer(blob);
        });
  
        const asset = runtime.storage.createAsset(
          assetType,
          dataType,
          new Uint8Array(arrayBuffer),
          null,
          true
        );
        const md5ext = `${asset.assetId}.${asset.dataFormat}`;
  
        try {
          await vm.addCostume(
            md5ext,
            {
              asset,
              md5ext,
              name: assetName,
            },
            targetId
          );
        } catch (e) {
          console.error(e);
      }
      }
    }
    
    async _handleGif(frameNumber, dataUrl, numFrames, assetName, targetId) {
      for (let i = 0; i < numFrames - 1; i++) {
        const frameRes = await Scratch.fetch(dataUrl);
        const frameArrayBuffer = await frameRes.arrayBuffer();
        
        const frameAssetType = runtime.storage.AssetType.ImageBitmap
        const frameDataType = runtime.storage.DataFormat.PNG

        const frameAsset = runtime.storage.createAsset(
          frameAssetType,
          frameDataType,
          new Uint8Array(frameArrayBuffer),
          null,
          true
        );
        const frame_md5ext = `${frameAsset.assetId}.${frameAsset.dataFormat}`;
  
        try {
          await vm.addCostume(
            frame_md5ext,
            {
              asset: frameAsset,
              md5ext: frame_md5ext,
              name: `${assetName}(${i})`,
            },
            targetId
          );
        } catch (e) {
          console.error(e);
        }
      }
    }

    async addSound(args, util) {
      const targetId = util.target.id;
      const assetName = Cast.toString(args.NAME);

      const res = await Scratch.fetch(args.URL);
      const buffer = await res.arrayBuffer();

      const storage = runtime.storage;
      const asset = storage.createAsset(
        storage.AssetType.Sound,
        storage.DataFormat.MP3,
        new Uint8Array(buffer),
        null,
        true
      );

      try {
        await vm.addSound(
          {
            asset,
            md5: asset.assetId + "." + asset.dataFormat,
            name: assetName,
          },
          targetId
        );
      } catch (e) {
        console.error(e);
      }
    }
    // End of PenguinMod

    renameSprite(args, util) {
      const target = this._getTargetFromMenu(args.TARGET, util);
      if (!target || target.isStage) return;

      const name = Cast.toString(args.NAME);
      target.sprite.name = name;
    }

    renameCostume(args, util) {
      const target = util.target;
      const costumeName = Cast.toString(args.COSTUME);
      const costumeIndex = target.getCostumeIndexByName(costumeName);
      if (costumeIndex < 0) return;

      const name = Cast.toString(args.NAME);
      target.renameCostume(costumeIndex, name);
    }

    renameSound(args, util) {
      const target = util.target;
      const soundName = Cast.toString(args.SOUND);
      const soundIndex = this._getSoundIndexByName(soundName, util);
      if (soundIndex < 0) return;

      const name = Cast.toString(args.NAME);
      target.renameSound(soundIndex, name);
    }

    deleteSprite(args, util) {
      const target = this._getTargetFromMenu(args.TARGET);
      if (!target || target.isStage) return;

      Scratch.vm.deleteSprite(target.id);
    }

    deleteCostume(args, util) {
      const target = util.target;
      const costumeName = Cast.toString(args.COSTUME);
      const costumeIndex = target.getCostumeIndexByName(costumeName);
      if (costumeIndex < 0) return;

      if (target.sprite.costumes.length > 0) {
        target.deleteCostume(costumeIndex);
      }
    }

    deleteSound(args, util) {
      const target = util.target;
      const soundName = Cast.toString(args.SOUND);
      const soundIndex = this._getSoundIndexByName(soundName, util);
      if (soundIndex < 0) return;

      if (target.sprite.sounds.length > 0) {
        target.deleteSound(soundIndex);
      }
    }

    getAllSprites() {
      const spriteNames = [];
      const targets = Scratch.vm.runtime.targets;
      for (const target of targets) {
        // People reckoned the stage shouldn't be included
        if (target.isOriginal && !target.isStage) {
          spriteNames.push(target.sprite.name);
        }
      }
      return JSON.stringify(spriteNames);
    }

    getAllCostumes(args, util) {
      const costumeNames = [];
      const costumes = util.target.sprite.costumes;
      for (const costume of costumes) {
        costumeNames.push(costume.name);
      }
      return JSON.stringify(costumeNames);
    }

    getAllSounds(args, util) {
      const soundNames = [];
      const sounds = util.target.sprite.sounds;
      for (const sound of sounds) {
        soundNames.push(sound.name);
      }
      return JSON.stringify(soundNames);
    }

    getSpriteName(args, util) {
      return util.target.sprite.name ?? "";
    }

    reorderCostume(args, util) {
      const target = util.target;
      const index1 = Cast.toNumber(args.INDEX1) - 1;
      const index2 = Cast.toNumber(args.INDEX2) - 1;
      const costumes = target.sprite.costumes;

      if (index1 < 0 || index1 >= costumes.length) return;
      if (index2 < 0 || index2 >= costumes.length) return;

      target.reorderCostume(index1, index2);
    }

    reorderSound(args, util) {
      const target = util.target;
      const index1 = Cast.toNumber(args.INDEX1) - 1;
      const index2 = Cast.toNumber(args.INDEX2) - 1;
      const sounds = target.sprite.sounds;

      if (index1 < 0 || index1 >= sounds.length) return;
      if (index2 < 0 || index2 >= sounds.length) return;

      target.reorderSound(index1, index2);
    }

    getCostumeData(args, util) {
      const target = util.target;
      const attribute = Cast.toString(args.ATTRIBUTE);
      const costumeName = Cast.toString(args.COSTUME);
      const costumeIndex = target.getCostumeIndexByName(costumeName);
      if (costumeIndex < 0) return "";

      const costume = target.sprite.costumes[costumeIndex];
      switch (attribute) {
        case "dataURI":
          if (!requireNonPackagedRuntime("dataURI of costume")) return "";
          return costume.asset.encodeDataURI();
        case "index":
          return costumeIndex + 1;
        case "format":
          if (!requireNonPackagedRuntime("format of costume")) return "";
          return costume.asset.assetType.runtimeFormat;
        case "header":
          if (!requireNonPackagedRuntime("header of costume")) return "";
          return costume.asset.assetType.contentType;
        case "asset ID":
          if (!requireNonPackagedRuntime("asset ID of costume")) return "";
          return costume.asset.assetId;
        default:
          return "";
      }
    }

    getSoundData(args, util) {
      const target = util.target;
      const attribute = Cast.toString(args.ATTRIBUTE);
      const soundName = Cast.toString(args.SOUND);
      const soundIndex = this._getSoundIndexByName(soundName, util);
      if (soundIndex < 0) return "";

      const sound = target.sprite.sounds[soundIndex];
      switch (attribute) {
        case "dataURI":
          if (!requireNonPackagedRuntime("dataURI of sound")) return "";
          return sound.asset.encodeDataURI();
        case "index":
          return soundIndex + 1;
        case "format":
          if (!requireNonPackagedRuntime("format of sound")) return "";
          return sound.asset.assetType.runtimeFormat;
        case "header":
          if (!requireNonPackagedRuntime("header of sound")) return "";
          return sound.asset.assetType.contentType;
        case "asset ID":
          if (!requireNonPackagedRuntime("asset ID of sound")) return "";
          return sound.asset.assetId;
        default:
          return "";
      }
    }

    getCostumeAtIndex(args, util) {
      const target = util.target;
      const index = Math.round(Cast.toNumber(args.INDEX - 1));
      const costumes = target.sprite.costumes;
      if (index < 0 || index >= costumes.length) return "";

      return costumes[index].name;
    }

    getSoundAtIndex(args, util) {
      const target = util.target;
      const index = Math.round(Cast.toNumber(args.INDEX - 1));
      const sounds = target.sprite.sounds;
      if (index < 0 || index >= sounds.length) return "";

      return sounds[index].name;
    }

    openProject(args) {
      const url = Cast.toString(args.URL);
      Scratch.fetch(url)
        .then((r) => r.arrayBuffer())
        .then((buffer) => vm.loadProject(buffer));
    }

    getProjectJSON() {
      return Scratch.vm.toJSON();
    }

    async loadExtension(args) {
      const url = Cast.toString(args.URL);
      await vm.extensionManager.loadExtensionURL(url);
    }

    getLoadedExtensions(args) {
      return JSON.stringify(
        Array.from(vm.extensionManager._loadedExtensions.keys())
      );
    }

    /* Utility Functions */

    _getSoundIndexByName(soundName, util) {
      const sounds = util.target.sprite.sounds;
      for (let i = 0; i < sounds.length; i++) {
        if (sounds[i].name === soundName) {
          return i;
        }
      }
      return -1;
    }

    // PenguinMod
    _typeIsBitmap(type) {
      return (
        type === "image/png" ||
        type === "image/bmp" ||
        type === "image/jpg" ||
        type === "image/jpeg" ||
        type === "image/jfif" ||
        type === "image/webp" ||
        type === "image/gif"
      );
    }

    _getTargetFromMenu(targetName, util) {
      let target = Scratch.vm.runtime.getSpriteTargetByName(targetName);
      if (targetName === "_myself_") target = util.target.sprite.clones[0];
      return target;
    }

    _getTargets() {
      const spriteNames = [];
      if (Scratch.vm.editingTarget && !Scratch.vm.editingTarget.isStage) {
        spriteNames.push({
          text: "myself",
          value: "_myself_",
        });
      }
      const targets = Scratch.vm.runtime.targets;
      for (let index = 1; index < targets.length; index++) {
        const target = targets[index];
        if (target.isOriginal) {
          spriteNames.push(target.getName());
        }
      }
      if (spriteNames.length > 0) {
        return spriteNames;
      } else {
        return [""];
      }
    }
  }
  Scratch.extensions.register(new Assets());
})(Scratch);