Convpath_length = function(sid, didvec){ #graph nodeID input
  minlength = 10000000
  minid = 0
  startingnodeid = sid
  rtypevec = c()
  
  for (i in 1:length(didvec)) {
    did = didvec[i]
    arrivingnodeid = did
    rtype = 0
    
    #same comp confirmation----------
    if(nodes[startingnodeid, ]$comp == nodes[arrivingnodeid, ]$comp){
      if(1 %in% suppressWarnings(st_intersection(edges, nodes[startingnodeid, ]))$level){
        vec1 = c(1,0)
      }else{
        vec1 = c(0,1)
      }
      if(1 %in% suppressWarnings(st_intersection(edges, nodes[arrivingnodeid, ]))$level){
        vec2 = c(1,0)
      }else{
        vec2 = c(0,1)
      }
      vec = c(vec1, vec2)
      
      #road network select------
      if (all(vec == c(1, 0, 1, 0))) { #both major----------------------------------------------------------------------
        rtype = 1
        path = shortest.paths(
          graph = subL1graph,
          v = startingnodeid,
          to = arrivingnodeid,
          mode = 'all',
          weights = subL1graph %>% activate(edges) %>% pull(length)
        )
        
        if(minlength > c(path)){
          minlength = c(path)
          minid = arrivingnodeid 
        }
        
      } else if(all(vec == c(1, 0, 0, 1))){ #entry major----------------------------------------------------------------
        rtype = 2
        h = shortest.paths(subL2graph, v = arrivingnodeid, to = transid, mode = "all", weights = subL2graph %>% activate(edges) %>% pull(length)) 
        targettransidlist = transid[which(!is.infinite(h))] 
        targettransidlistordered = targettransidlist[order(h[,!is.infinite(h)])]
        
        i=1
        repeat{
          targettransid = targettransidlistordered[i]
          
          path_maj = shortest.paths(
            graph = subL1graph,
            v = startingnodeid,
            to = targettransid,
            mode = "all",
            weights = subL1graph %>% activate(edges) %>% pull(length)
          )
          if(!is.infinite(path_maj)){
            break
          }
          i=i+1
        }
        
        path_min = shortest.paths(
          graph = subL2graph,
          v = targettransid,
          to = arrivingnodeid,
          mode = "all",
          weights = subL2graph %>% activate(edges) %>% pull(length)
        )
        
        path = c(path_maj + path_min)
        if(minlength > c(path)){
          minlength = c(path)
          minid = did
        }
        
      } else if(all(vec == c(0, 1, 1, 0))){ #exit major---------------
        rtype = 3
        h = shortest.paths(subL2graph, v = startingnodeid, to = transid, mode = "all", weights = subL2graph %>% activate(edges) %>% pull(length)) 
        targettransidlist = transid[which(!is.infinite(h))] #목표 transnode 리스트
        targettransidlistordered = targettransidlist[order(h[,!is.infinite(h)])]
        
        i=1
        repeat{
          targettransid = targettransidlistordered[i]
          
          path_maj = shortest.paths(
            graph = subL1graph,
            v = targettransid,
            to = arrivingnodeid,
            mode = "all",
            weights = subL1graph %>% activate(edges) %>% pull(length)
          )
          if(!is.infinite(path_maj)){
            break
          }
          i=i+1
        }
        
        path_min = shortest.paths(
          graph = subL2graph,
          v = startingnodeid,
          to = targettransid,
          mode = "all",
          weights = subL2graph %>% activate(edges) %>% pull(length)
        )
        
        path = c(path_min + path_maj)
        if(minlength > c(path)){
          minlength = c(path)
          minid = did
        }
        
      } else if(all(vec == c(0, 1, 0, 1))){
        path = shortest_paths(
          graph = graph,
          from = startingnodeid,
          to = arrivingnodeid,
          output = 'both',
          weights = graph %>% activate(edges) %>% pull(length)
        )
        
        path_graph = graph %>% subgraph.edges(eids = path$epath %>% unlist()) %>% as_tbl_graph()
        
        order = rank(path$epath %>% unlist())
        edgeseq = graph %>% activate(edges) %>% slice(path$epath %>% unlist()) %>% pull(edgeID)
        edgeseq = edgeseq[order]
        
        if(1 %in% edges$level[edgeseq]){
          #(case2)major in the middle
          rtype = 4
          hs = shortest.paths(subL2graph, v = startingnodeid, to = transid, mode = "all", weights = subL2graph %>% activate(edges) %>% pull(length)) 
          targettransidlist_s = transid[which(!is.infinite(hs))] #target transnode lists
          targettransidlistordered_s = targettransidlist_s[order(hs[,!is.infinite(hs)])]
          
          hd = shortest.paths(subL2graph, v = arrivingnodeid, to = transid, mode = "all", weights = subL2graph %>% activate(edges) %>% pull(length)) 
          targettransidlist_d = transid[which(!is.infinite(hd))] #target transnode lists
          targettransidlistordered_d = targettransidlist_d[order(hd[,!is.infinite(hd)])]
          
          #all combinations
          combis = expand.grid(1:length(targettransidlist_s), 1:length(targettransidlist_d))
          
          #repeat
          i=1
          repeat{
            targettransid_s = targettransidlistordered_s[as.numeric(combis[i,][1])]
            targettransid_d = targettransidlistordered_d[as.numeric(combis[i,][2])]
            
            path_maj = shortest.paths(
              graph = subL1graph,
              v = targettransid_s,
              to = targettransid_d,
              mode = "all",
              weights = subL1graph %>% activate(edges) %>% pull(length))
            if(!is.infinite(path_maj)){
              break
            }
            i=i+1
          }
          
          path_min_s = shortest.paths(
            graph = subL2graph,
            v = startingnodeid,
            to = targettransid_s,
            mode = "all",
            weights = subL2graph %>% activate(edges) %>% pull(length))
          path_min_d = shortest.paths(
            graph = subL2graph,
            v = targettransid_d,
            to = arrivingnodeid,
            mode = "all",
            weights = subL2graph %>% activate(edges) %>% pull(length))
          
          path = c(path_min_s + path_maj + path_min_d)
          if(minlength > c(path)){
            minlength = c(path)
            minid = did
          }
          
        }else{
          #(case1)only minor route
          rtype = 5
          path_min = shortest.paths(
            graph = subL2graph,
            v = startingnodeid,
            to = arrivingnodeid,
            mode = "all",
            weights = subL2graph %>% activate(edges) %>% pull(length)
          )
          
          if(minlength > c(path_min)){
            minlength = c(path_min)
            minid = did
          }
        }
      }
      
    }else{
      if(minlength == 10000000){
        minlength = 20000000
      }
    } #--------- if end
    rtypevec[i] = rtype
  }#------- for end
  index = which(didvec == minid)
  minrtype = rtypevec[index]
  
  return(list(minid, minlength, minrtype)) 
}
