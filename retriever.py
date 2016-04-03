# -*- coding: utf-8 -*-
import sqlite3
import spotipy
import spotipy.util as util
from spotipy.oauth2 import SpotifyClientCredentials
import pandas as pd

from config import *

import requests.packages.urllib3
requests.packages.urllib3.disable_warnings()

def get_songs(sp, playlist):
        """
        Returns the list of all songs (individual tracks) contained inside a playlist.
        """
        try:
            track_results = sp.user_playlist(playlist[1], 
                                             playlist[0])   
        except:
            track_results = None
            
        songs = []
        
        if not track_results or not 'tracks' in track_results:
            return songs
            
        cont = True
        uri = track_results['uri']
        name = track_results['name'] if 'name' in track_results else ''
        track_results = track_results['tracks']
        while cont and track_results:
            print "Still getting more songs for %s" % uri
            songs = songs + [x for x in track_results['items'] if x is not None]
            if track_results['next']:
                try:
                    track_results = sp.next(track_results)
                except:
                    cont = False
            else:
                cont = False

        if len(songs) > 1:
            # Filter it here. Return a list of dictionaries with the 
            # desired properties
            try:
                songs = [dict(title = song['track']['name'],
                              artist = song['track']['artists'][0]['name'],
                              artitle = song['track']['name'] + ' - ' + \
                                        song['track']['artists'][0]['name'],
                              uri = uri,
                              name = name)
                        for song in songs]
            except:
                songs = []
        else:
            songs = []
            
        return(songs)
               
def store_songs(terms, dbfile, total_playlists = 1):
                                               
    scc = SpotifyClientCredentials(client_id = SPOTIPY_CLIENT_ID,
                                   client_secret = SPOTIPY_CLIENT_SECRET)
                        
    sp = spotipy.Spotify(client_credentials_manager = scc)
    
    analyzed = set()
    nsongs = 0
    offset = 0
    chunk_size = 10
    n_playlists = 0
    
    for term in terms:
        print "Starting search with %s" % term
        offset = 0
        
        while n_playlists < total_playlists:
            sp = spotipy.Spotify(client_credentials_manager = scc)
            try:
                playlists = sp.search(q = term, limit = chunk_size, 
                                      offset = offset, type = "playlist")
            except:
                offset = offset + 1
                continue
                
            offset = offset + chunk_size
            
            if not playlists or len(playlists['playlists']['items']) == 0:
                print "There are no more playlists"
                break
            else:
                print "Got %d playlists" % len(playlists['playlists']['items'])
                    
            playlists = [(x['uri'], x['owner']['id']) for x in playlists['playlists']['items']]
            for playlist in playlists:
                n_playlists += 1
                
                # Make sure we don't iterate through playlists already in the DB
                if playlist[0] not in analyzed:                
                    analyzed.add(playlist[0])
                    songs = get_songs(sp, playlist)
                    if songs and len(songs) > 0:
                        df = pd.DataFrame(songs)
                        df.drop_duplicates(inplace = True)
                        nsongs += df.shape[0]
                        print "[Term: %s] Have retrieved %d songs from %d playlists" % \
                        (term, nsongs, n_playlists)                
                        df['term'] = term
                        conn = sqlite3.connect(dbfile)
                        df.to_sql("songs", conn, if_exists = "append")
                        conn.close()
    
if __name__ == "__main__":
    import sys
    dbfile = sys.argv[1]
    terms = sys.argv[2:]
    total_number = 100000
    store_songs(terms, dbfile, total_number)
