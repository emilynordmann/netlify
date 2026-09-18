"""Minimal FIT -> CSV decoder (no dependencies). Extracts record, lap, session, event messages."""
import struct, sys, csv, json, datetime

BASE = {0:('B',1),1:('b',1),2:('B',1),3:('h',2),4:('H',2),5:('i',4),6:('I',4),7:('s',1),
        8:('f',4),9:('d',8),10:('B',1),11:('H',2),12:('I',4),13:('B',1),14:('q',8),15:('Q',8),16:('Q',8)}
INVALID = {0:0xFF,1:0x7F,2:0xFF,3:0x7FFF,4:0xFFFF,5:0x7FFFFFFF,6:0xFFFFFFFF,10:0xFF,11:0xFFFF,12:0xFFFFFFFF,13:0xFF}

RECORD = {253:'timestamp',0:'lat',1:'lon',2:'altitude',3:'hr',4:'cadence',5:'distance',6:'speed',
          13:'temperature',73:'enh_speed',78:'enh_altitude',39:'vertical_oscillation',
          41:'stance_time',53:'fractional_cadence',90:'performance_condition'}
SESSION = {253:'timestamp',2:'start_time',7:'total_elapsed_time',8:'total_timer_time',9:'total_distance',
           11:'total_calories',14:'avg_speed',15:'max_speed',16:'avg_hr',17:'max_hr',18:'avg_cadence',
           22:'total_ascent',23:'total_descent',5:'sport',6:'sub_sport',3:'start_lat',4:'start_lon',
           38:'nec_lat',39:'nec_lon',40:'swc_lat',41:'swc_lon',57:'avg_temperature',58:'max_temperature',
           124:'enh_avg_speed',125:'enh_max_speed',110:'total_moving_time',
           149:'min_altitude',150:'max_altitude',126:'avg_altitude',128:'max_altitude_old',33:'num_laps',
           89:'avg_vertical_oscillation',90:'avg_stance_time',44:'avg_pos_grade',45:'avg_neg_grade'}
LAP = {253:'timestamp',2:'start_time',7:'total_elapsed_time',8:'total_timer_time',9:'total_distance',
       15:'avg_hr',16:'max_hr',21:'total_ascent',22:'total_descent',13:'avg_speed',14:'max_speed',
       3:'start_lat',4:'start_lon',5:'end_lat',6:'end_lon',11:'total_calories',24:'intensity',25:'lap_trigger'}
EVENT = {253:'timestamp',0:'event',1:'event_type',3:'data',4:'event_group'}
SEMI = 180/2**31
EPOCH = datetime.datetime(1989,12,31,tzinfo=datetime.timezone.utc)

def decode(path):
    data = open(path,'rb').read()
    hsize = data[0]
    pos = hsize
    end = hsize + struct.unpack('<I', data[4:8])[0]
    defs = {}
    out = {'record':[], 'lap':[], 'session':[], 'event':[], 'other':{}}
    while pos < end:
        hdr = data[pos]; pos += 1
        if hdr & 0x80:  # compressed timestamp header
            local = (hdr >> 5) & 3
        else:
            local = hdr & 0x0F
        if (hdr & 0x40) and not (hdr & 0x80):  # definition
            has_dev = bool(hdr & 0x20)
            arch = data[pos+1]; en = '<' if arch == 0 else '>'
            gnum = struct.unpack(en+'H', data[pos+2:pos+4])[0]
            nf = data[pos+4]; pos += 5
            fields = []
            for i in range(nf):
                fnum, size, btype = data[pos], data[pos+1], data[pos+2]; pos += 3
                fields.append((fnum, size, btype & 0x1F))
            devf = []
            if has_dev:
                nd = data[pos]; pos += 1
                for i in range(nd):
                    devf.append((data[pos], data[pos+1], data[pos+2])); pos += 3
            defs[local] = (gnum, en, fields, devf)
        else:
            gnum, en, fields, devf = defs[local]
            vals = {}
            for fnum, size, bt in fields:
                raw = data[pos:pos+size]; pos += size
                fmt, bsz = BASE.get(bt, ('B',1))
                if bt == 7:
                    v = raw.split(b'\0')[0].decode('utf8','ignore')
                elif size % bsz == 0 and size // bsz > 1:
                    v = struct.unpack(en+fmt*(size//bsz), raw)
                elif size == bsz:
                    v = struct.unpack(en+fmt, raw)[0]
                    if bt in INVALID and v == INVALID[bt]: v = None
                else:
                    v = raw
                vals[fnum] = v
            for dn, size, dt in devf:
                pos += size
            if gnum == 20:
                r = {RECORD.get(k, f'f{k}'): v for k, v in vals.items()}
                out['record'].append(r)
            elif gnum == 19:
                out['lap'].append({LAP.get(k, f'f{k}'): v for k, v in vals.items()})
            elif gnum == 18:
                out['session'].append({SESSION.get(k, f'f{k}'): v for k, v in vals.items()})
            elif gnum == 21:
                out['event'].append({EVENT.get(k, f'f{k}'): v for k, v in vals.items()})
            else:
                out['other'][gnum] = out['other'].get(gnum, 0) + 1
    return out

def ts(v): return (EPOCH + datetime.timedelta(seconds=v)).isoformat() if v is not None else None

if __name__ == '__main__':
    src, dst = sys.argv[1], sys.argv[2]
    o = decode(src)
    rows = []
    for r in o['record']:
        if r.get('timestamp') is None: continue
        alt = r.get('enh_altitude'); alt = alt/5 - 500 if alt is not None else (r['altitude']/5-500 if r.get('altitude') is not None else None)
        spd = r.get('enh_speed'); spd = spd/1000 if spd is not None else (r['speed']/1000 if r.get('speed') is not None else None)
        rows.append({'time': ts(r['timestamp']),
                     'lat': r['lat']*SEMI if r.get('lat') is not None else None,
                     'lon': r['lon']*SEMI if r.get('lon') is not None else None,
                     'ele': alt, 'dist_m': r['distance']/100 if r.get('distance') is not None else None,
                     'speed_ms': spd, 'hr': r.get('hr'), 'cadence': r.get('cadence'), 'temp': r.get('temperature')})
    with open(dst, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys())); w.writeheader(); w.writerows(rows)
    s = o['session'][0] if o['session'] else {}
    summ = {k: (ts(v) if k in ('timestamp','start_time') else v) for k, v in s.items()}
    print(json.dumps({'n_records': len(rows), 'n_laps': len(o['lap']), 'n_events': len(o['event']),
                      'other_msgs': o['other'], 'session': summ}, indent=1, default=str))
    for e in o['event'][:40]: print('event', e)
    for l in o['lap'][:20]: print('lap', {k: (ts(v) if k in ('timestamp','start_time') else v) for k,v in l.items()})
