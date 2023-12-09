#! /bin/awk -f

BEGIN {
    total = 0;
}

{
  arr_len = split($0, fields, " ");
  if(part == "2") {
    reverseArray(fields, new_fields)
    total += process_data(new_fields, arr_len);
  } else {
    total += process_data(fields, arr_len);
  }
}

function process_data(data, data_len) {
  if (data_len == 0){
    return 0;
  }
  s = 0
  for (i = 1; i < data_len; i++) {
    data[i] = data[i+1] - data[i]
    s = data[i] == 0 && data[i];
  }
  if (s != 0) {
    return data[data_len]
  }
  return data[data_len] + process_data(data, data_len -1)
  
}
# function to reverse an array
function reverseArray(inputArray, outputArray, len) {
    len = length(inputArray);
    for (i = len; i > 0; i--) {
        outputArray[len - i + 1] = inputArray[i];
    }
}

END {
  print total
}
