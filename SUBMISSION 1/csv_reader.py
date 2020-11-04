import csv


def csv_reader(file):
    with open(file, 'r') as f:
        data_dict = dict()
        reader = csv.reader(f)
        dates = []
        opns = []
        highs = []
        lows = []
        closes = []
        adj_closes = []
        volumes = []
        for date, opn, high, low, close, adj_close, volume in reader:
            try:
                dates.append(date)
                opns.append(float(opn))
                highs.append(float(high))
                lows.append(float(low))
                closes.append(float(close))
                adj_closes.append(float(adj_close))
                volumes.append(float(volume))
            except ValueError:
                continue
        data_dict['date'] = dates[0:]
        data_dict['open'] = opns[0:]
        data_dict['close'] = closes[0:]
        data_dict['high'] = highs[0:]
        data_dict['low'] = lows[0:]
        data_dict['adj_close'] = adj_closes[0:]
        data_dict['volume'] = volumes[0:]
    return data_dict

