from bing_image_downloader import downloader
downloader.download("panda", limit=200,  
                    adult_filter_off=True, force_replace=False, timeout=60)
