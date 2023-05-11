// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#pragma once

#include "compiler.h"

//  The "callback" object needs to implement the following member
//  functions that the "flow" object will call as the data flow
//  analysis progresses:
//
//  class Callback
//  {
//  public:
//      void StartMerge(BasicBlock* block);
//      void Merge(BasicBlock* block, BasicBlock* pred, unsigned dupCount);
//      void MergeHandler(BasicBlock* block, BasicBlock* firstTryBlock, BasicBlock* lastTryBlock);
//      bool EndMerge(BasicBlock* block);
//  };

template <typename TCallback>
void ForwardDataFlow(TCallback&& callback, Compiler* compiler)
{
    jitstd::list<BasicBlock*> worklist(jitstd::allocator<void>(compiler->getAllocator()));

    worklist.insert(worklist.begin(), compiler->fgFirstBB);
    while (!worklist.empty())
    {
        BasicBlock* block = *(worklist.begin());
        worklist.erase(worklist.begin());

        callback.StartMerge(block);

        if (compiler->bbIsHandlerBeg(block))
        {
            EHblkDsc* ehDsc = compiler->ehGetBlockHndDsc(block);
            callback.MergeHandler(block, ehDsc->ebdTryBeg, ehDsc->ebdTryLast);
        }
        else
        {
            assert(block->bbPreds == compiler->BlockPredsWithEH(block));

            for (flowList* pred = block->bbPreds; pred; pred = pred->flNext)
            {
                callback.Merge(block, pred->getBlock(), pred->flDupCount);
            }
        }

        if (callback.EndMerge(block))
        {
            for (BasicBlock* succ : block->GetAllSuccs(compiler))
            {
                worklist.insert(worklist.end(), succ);
            }
        }
    }
}
