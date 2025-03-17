///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           

/// INTENTIONALLY NOT GUARDED                                                 
/// Include this file once in each cpp file, after all other headers          
#include <Langulus/Anyness/Text.hpp>
#include <Langulus/Anyness/Pair.hpp>
#include <Langulus/Anyness/TPair.hpp>
#include "../many/TestManyCommon.hpp"
#include <unordered_map>


///                                                                           
/// Possible states:                                                          
///   - uninitialized                                                         
///   - default                                                               
template<class K, class V>
void Pair_CheckState_Default(const auto&);
///   - invariant                                                             
template<class K, class V>
void Pair_CheckState_Invariant(const auto&);
///   - owned-full                                                            
template<class K, class V>
void Pair_CheckState_OwnedFull(const auto&);
///   - owned-full-const                                                      
template<class K, class V>
void Pair_CheckState_OwnedFullConst(const auto&);
///   - owned-empty                                                           
template<class K, class V>
void Pair_CheckState_OwnedEmpty(const auto&);
///   - disowned-full                                                         
template<class K, class V>
void Pair_CheckState_DisownedFull(const auto&);
///   - disowned-full-const                                                   
template<class K, class V>
void Pair_CheckState_DisownedFullConst(const auto&);
///   - abandoned                                                             
template<class K, class V>
void Pair_CheckState_Abandoned(const auto&);


template<class K, class V>
void Pair_CheckState_Default(const auto& pair) {
   if constexpr (CT::Typed<decltype(pair)>) {
      Many_CheckState_DisownedFullConst<K>(pair.GetKeyBlock());
      Many_CheckState_DisownedFullConst<V>(pair.GetValueBlock());
   }
   else {
      Many_CheckState_Default<K>(pair.GetKeyBlock());
      Many_CheckState_Default<V>(pair.GetValueBlock());
   }
}

template<class K, class V>
void Pair_CheckState_OwnedFull(const auto& pair) {
   if constexpr (CT::Typed<decltype(pair)>) {
      Many_CheckState_DisownedFullConst<K>(pair.GetKeyBlock());
      Many_CheckState_DisownedFullConst<V>(pair.GetValueBlock());
   }
   else {
      Many_CheckState_OwnedFull<K>(pair.GetKeyBlock());
      Many_CheckState_OwnedFull<V>(pair.GetValueBlock());
   }
}

template<class K, class V>
void Pair_CheckState_OwnedEmpty(const auto& pair) {
   if constexpr (CT::Typed<decltype(pair)>) {
      Many_CheckState_DisownedFullConst<K>(pair.GetKeyBlock());
      Many_CheckState_DisownedFullConst<V>(pair.GetValueBlock());
   }
   else {
      Many_CheckState_OwnedEmpty<K>(pair.GetKeyBlock());
      Many_CheckState_OwnedEmpty<V>(pair.GetValueBlock());
   }
}